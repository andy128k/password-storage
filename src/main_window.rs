use crate::actions;
use crate::actions::*;
use crate::cache::Cache;
use crate::config::Config;
use crate::error::*;
use crate::format;
use crate::model::record::{Record, RecordType, FIELD_NAME, RECORD_TYPES};
use crate::model::tree::RecordTree;
use crate::store::PSStore;
use crate::ui;
use crate::ui::dashboard::PSDashboard;
use crate::ui::dialogs::ask::ask;
use crate::ui::dialogs::ask_save::{ask_save, AskSave};
use crate::ui::dialogs::change_password::change_password;
use crate::ui::dialogs::open_file::open_file;
use crate::ui::dialogs::read_file::read_file;
use crate::ui::dialogs::save_file::save_file;
use crate::ui::dialogs::say::{say_error, say_info};
use crate::ui::edit_record::edit_record;
use crate::ui::filter::create_model_filter;
use crate::ui::preview_panel::PSPreviewPanel;
use crate::ui::search::PSSearchEntry;
use crate::ui::tree_view::PSTreeView;
use crate::utils::clipboard::get_clipboard;
use gio::prelude::*;
use gio::{SimpleAction, SimpleActionGroup};
use glib::clone;
use gtk::prelude::*;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::convert::Into;
use std::iter::Iterator;
use std::path::{Path, PathBuf};
use std::rc::Rc;

enum AppMode {
    Initial,
    FileOpened,
    MergeMode,
}

struct PSMainWindowPrivate {
    mode: Cell<AppMode>,
    stack: gtk::Stack,
    dashboard: PSDashboard,
    data: RefCell<PSStore>,
    view: PSTreeView,
    preview: PSPreviewPanel,

    actions: RefCell<HashMap<PSAction, SimpleAction>>,

    search_entry: PSSearchEntry,

    statusbar: gtk::Statusbar,

    filename: RefCell<Option<PathBuf>>,
    password: RefCell<Option<String>>,
    changed: Cell<bool>,

    config: Rc<RefCell<Config>>,
    cache: Cache,
}

#[derive(Clone)]
pub struct PSMainWindow(gtk::ApplicationWindow);

pub struct PSMainWindowWeak(<gtk::ApplicationWindow as glib::clone::Downgrade>::Weak);

impl glib::clone::Downgrade for PSMainWindow {
    type Weak = PSMainWindowWeak;

    fn downgrade(&self) -> Self::Weak {
        PSMainWindowWeak(glib::clone::Downgrade::downgrade(&self.0))
    }
}

impl glib::clone::Upgrade for PSMainWindowWeak {
    type Strong = PSMainWindow;

    fn upgrade(&self) -> Option<Self::Strong> {
        glib::clone::Upgrade::upgrade(&self.0).map(PSMainWindow)
    }
}

fn set_status(win: &PSMainWindow, message: &str) {
    let statusbar = &win.private().statusbar;
    statusbar.pop(0);
    statusbar.push(0, message);
}

impl PSMainWindow {
    pub fn from_window(window: &gtk::Window) -> Option<Self> {
        let app_window = window.clone().downcast::<gtk::ApplicationWindow>().ok()?;
        if !unsafe { *app_window.get_data("is_main_window").unwrap_or(&false) } {
            return None;
        }
        Some(Self(app_window))
    }

    fn private(&self) -> &PSMainWindowPrivate {
        unsafe { self.0.get_data("private").unwrap() }
    }

    pub fn window(&self) -> gtk::Window {
        self.0.clone().upcast()
    }

    fn ensure_data_is_saved(&self) -> bool {
        if !self.private().changed.get() {
            return true;
        }
        let window = self.window();
        match ask_save(
            &window,
            "Save changes before closing? If you don't save, changes will be permanently lost.",
        ) {
            AskSave::Save => {
                if let Err(error) = cb_save(self) {
                    say_error(&window, &error.to_string());
                    false
                } else {
                    true
                }
            }
            AskSave::Discard => true,
            AskSave::Cancel => false,
        }
    }

    pub fn close(&self) {
        self.0.close();
    }

    fn refilter(&self) {
        if let Some(model) = self.private().view.view.get_model() {
            if let Ok(filter) = model.downcast::<gtk::TreeModelFilter>() {
                filter.refilter();
            }
        }
    }
}

fn get_selected_group_iter(win: &PSMainWindow) -> Option<gtk::TreeIter> {
    let model = &win.private().data.borrow();
    let selection = win.private().view.get_selected_iter();
    if let Some((iter, _path)) = selection {
        for (i, record) in model.parents(&iter) {
            if record.record_type.is_group {
                return Some(i);
            }
        }
    }
    None
}

fn listview_cursor_changed(win: &PSMainWindow, record: Option<Record>) {
    for (action_name, action) in win.private().actions.borrow().iter() {
        if let PSAction::Record(ref record_action_name) = *action_name {
            let enabled = match *record_action_name {
                RecordAction::CopyName => record.is_some(),
                RecordAction::CopyPassword => record.as_ref().and_then(|e| e.password()).is_some(),
                RecordAction::Edit => record.is_some(),
                RecordAction::Delete => record.is_some(),
                RecordAction::ConvertTo(ref to) => {
                    if let Some(ref e) = record {
                        !e.record_type.is_group && e.record_type.name != to
                    } else {
                        false
                    }
                }
            };
            action.set_enabled(enabled);
        }
    }

    let show_secrets_on_preview = win.private().config.borrow().show_secrets_on_preview;
    win.private()
        .preview
        .update(record, show_secrets_on_preview);
}

fn get_usernames(win: &PSMainWindow) -> Vec<String> {
    fn traverse(
        store: &PSStore,
        parent_iter: Option<&gtk::TreeIter>,
        result: &mut HashSet<String>,
    ) {
        for (i, record) in store.children(parent_iter) {
            if record.record_type.is_group {
                traverse(store, Some(&i), result);
            } else {
                record
                    .username()
                    .map(|username| result.insert(username.to_string()));
            }
        }
    }

    let mut result = HashSet::new();
    traverse(&win.private().data.borrow(), None, &mut result);
    result.remove("");

    let mut vec: Vec<String> = result.into_iter().collect();
    vec.sort();
    vec
}

fn cb_add_record(win: &PSMainWindow, record_type: &'static RecordType) -> Result<()> {
    let window = &win.window();

    let empty_record = record_type.new_record();
    if let Some(new_record) = edit_record(&empty_record, window, "Add", &get_usernames(win)) {
        let group_iter = get_selected_group_iter(win);
        let iter = win
            .private()
            .data
            .borrow()
            .append(group_iter.as_ref(), &new_record);
        win.refilter();
        win.private().view.select_iter(&iter);
        set_status(win, "New entry was added");
        win.private().changed.set(true);
    }
    Ok(())
}

fn cb_edit_record(win: &PSMainWindow) {
    let selection = win.private().view.get_selected_iter();
    if let Some((iter, _path)) = selection {
        let record_opt = win.private().data.borrow().get(&iter);
        if let Some(record) = record_opt {
            let window = win.window();
            if let Some(new_record) = edit_record(&record, &window, "Edit", &get_usernames(win)) {
                win.private().data.borrow().update(&iter, &new_record);
                win.refilter();
                listview_cursor_changed(win, Some(new_record));
                set_status(win, "Entry was changed");
                win.private().changed.set(true);
            }
        }
    }
}

fn cb_convert_record(win: &PSMainWindow, dest_record_type: &'static RecordType) -> Result<()> {
    let selection = win.private().view.get_selected_iter();
    if let Some((iter, _path)) = selection {
        let record_opt = win.private().data.borrow().get(&iter);
        if let Some(record) = record_opt {
            let new_record = {
                let mut new_record = dest_record_type.new_record();
                let name = record.get_field(&FIELD_NAME);
                new_record.set_field(&FIELD_NAME, &name);
                new_record.join(&record);
                new_record
            };
            win.private().data.borrow().update(&iter, &new_record);
            win.refilter();
            set_status(win, "Entry has changed type");
            win.private().changed.set(true);
            listview_cursor_changed(win, Some(new_record));
        }
    }
    Ok(())
}

fn cb_delele_record(win: &PSMainWindow) -> Result<()> {
    let selection = win.private().view.get_selected_iter();
    if let Some((iter, _path)) = selection {
        if ask(
            &win.window(),
            "Do you really want to delete selected entry?",
        ) {
            win.private().data.borrow().delete(&iter);
            listview_cursor_changed(win, None);
            set_status(win, "Entry was deleted");
            win.private().changed.set(true);
        }
    }
    Ok(())
}

fn cb_uncheck_all(win: &PSMainWindow) -> Result<()> {
    win.private().data.borrow().uncheck_all();
    set_status(win, "Unchecked all items");
    Ok(())
}

fn cb_merge(win: &PSMainWindow) -> Result<()> {
    let checked = {
        fn collect_checked(
            model: &PSStore,
            parent: Option<&gtk::TreeIter>,
            path: &[String],
            result: &mut Vec<(Record, Vec<String>)>,
        ) {
            for (i, record) in model.children(parent) {
                if model.is_selected(&i) {
                    result.push((record.clone(), path.to_vec()));
                }
                let mut p = path.to_vec();
                p.push(record.name().to_string());
                collect_checked(model, Some(&i), &p, result);
            }
        }
        let mut checked = Vec::new();
        collect_checked(
            &win.private().data.borrow(),
            None,
            &Vec::new(),
            &mut checked,
        );
        checked
    };

    if checked.len() < 2 {
        say_info(
            &win.window(),
            "Nothing to merge. Select few items and try again.",
        );
        return Ok(());
    }

    // rename
    let records = {
        let mut renamed_records: Vec<Record> = Vec::new();
        for &(ref record, ref path) in &checked {
            let mut renamed = record.clone();
            renamed.rename(move |old_name| format!("{}/{}", path.join("/"), old_name));
            renamed_records.push(renamed);
        }
        renamed_records
    };

    {
        let mut message = String::from("Do you want to merge following items?\n");
        for record in &records {
            message.push('\n');
            message.push_str(&record.name());
        }

        if !ask(&win.window(), &message) {
            return Ok(());
        }
    }

    // delete entries
    win.private().data.borrow().delete_checked();

    // create new entry
    let result = Record::join_entries(&records);

    // TODO: detect common path
    let iter = win.private().data.borrow().append(None, &result);
    win.refilter();
    win.private().view.select_iter(&iter);
    set_status(win, "New entry was created by merging");
    win.private().changed.set(true);
    Ok(())
}

fn load_data(filename: &Path, parent_window: &gtk::Window) -> Option<(RecordTree, String)> {
    read_file(parent_window, |password| {
        format::revelation::load_revelation_file(filename, password)
    })
}

fn new_password(parent_window: &gtk::Window) -> Option<String> {
    // TODO: ADD confirmation
    let mut form = ui::forms::form::Form::new();
    form.add(
        "Password",
        Box::new(ui::forms::entry::Password::new()),
        true,
    );
    let result = ui::edit_object::edit_object(
        None,
        form,
        parent_window,
        "Enter password",
        "password-storage",
    );
    result.map(|mut values| values.remove(0))
}

fn ensure_password_is_set(win: &PSMainWindow) -> Option<String> {
    if win.private().password.borrow().is_none() {
        let window = &win.window();
        *win.private().password.borrow_mut() = new_password(window);
    }
    win.private().password.borrow().clone()
}

fn save_data(win: &PSMainWindow, filename: &Path) -> Result<()> {
    if let Some(password) = ensure_password_is_set(win) {
        let tree = win.private().data.borrow().to_tree();

        format::revelation::save_revelation_file(filename, &password, &tree)?;

        set_status(
            win,
            &format!("File '{}' was saved", filename.to_string_lossy()),
        );
        win.private().changed.set(false);
        win.private().cache.add_file(filename);
    }
    Ok(())
}

const WINDOW_TITLE: &str = "Password Storage";

fn set_filename(win: &PSMainWindow, filename: Option<&Path>) {
    match filename {
        Some(filename) => {
            *win.private().filename.borrow_mut() = Some(filename.to_owned());
            win.window().set_title(&format!(
                "{} - {}",
                WINDOW_TITLE,
                &filename.to_string_lossy()
            ));
        }
        None => {
            *win.private().filename.borrow_mut() = None;
            win.window().set_title(WINDOW_TITLE);
        }
    }
}

impl PSMainWindow {
    pub fn new_file(&self) {
        if self.ensure_data_is_saved() {
            self.private().changed.set(false);

            let data = PSStore::new();
            *self.private().data.borrow_mut() = data.clone();
            self.private().view.set_model(&data.as_model());

            set_filename(self, None);
            self.private().search_entry.set_text("");
            *self.private().password.borrow_mut() = None;
            listview_cursor_changed(self, None);
            set_status(self, "New file was created");
            set_mode(self, AppMode::FileOpened);
        }
    }
}

pub fn do_open_file(win: &PSMainWindow, filename: &Path) {
    let window = &win.window();
    if let Some((entries, password)) = load_data(filename, window) {
        win.private().cache.add_file(filename);
        win.private().search_entry.set_text("");

        let data = PSStore::from_tree(&entries);
        *win.private().data.borrow_mut() = data.clone();
        win.private().view.set_model(&data.as_model());

        {
            set_filename(win, Some(filename));
            *win.private().password.borrow_mut() = Some(password);
            win.private().changed.set(false);
        }
        set_mode(win, AppMode::FileOpened);
        set_status(
            win,
            &format!("File '{}' was opened", filename.to_string_lossy()),
        );
        win.private().search_entry.grab_focus();
    }
}

impl PSMainWindow {
    pub fn open_file(&self) {
        if self.ensure_data_is_saved() {
            let window = self.window();
            if let Some(filename) = open_file(&window) {
                do_open_file(self, &filename);
            }
        }
    }
}

fn cb_merge_file(win: &PSMainWindow) -> Result<()> {
    win.private().search_entry.set_text("");

    let window = win.window();
    if let Some(filename) = open_file(&window) {
        if let Some((extra_records, _password)) = load_data(&filename, &window) {
            let mut records_tree = win.private().data.borrow().to_tree();
            crate::model::merge_trees::merge_trees(&mut records_tree, &extra_records);

            let data = PSStore::from_tree(&records_tree);
            *win.private().data.borrow_mut() = data.clone();
            win.private().view.set_model(&data.as_model());

            win.private().changed.set(true);
        }
    }
    Ok(())
}

fn cb_save_as(win: &PSMainWindow) -> Result<()> {
    let window = win.window();
    if let Some(ref filename) = save_file(&window) {
        set_filename(win, Some(filename));
        save_data(win, filename)?;
    }
    Ok(())
}

fn cb_save(win: &PSMainWindow) -> Result<()> {
    let filename = win.private().filename.borrow().clone();
    if let Some(ref filename) = filename {
        save_data(win, filename)?;
    } else {
        cb_save_as(win)?;
    }
    Ok(())
}

fn cb_close(win: &PSMainWindow) -> Result<()> {
    if win.ensure_data_is_saved() {
        win.private().changed.set(false);
        win.private().search_entry.set_text("");

        let data = PSStore::new();
        *win.private().data.borrow_mut() = data.clone();
        win.private().view.set_model(&data.as_model());

        set_filename(win, None);
        *win.private().password.borrow_mut() = None;
        listview_cursor_changed(win, None);
        set_mode(win, AppMode::Initial);
        set_status(win, "");
    }
    Ok(())
}

fn cb_change_password(win: &PSMainWindow) -> Result<()> {
    let window = win.window();
    if let Some(new_password) = change_password(&window) {
        *win.private().password.borrow_mut() = Some(new_password);
        win.private().changed.set(true);
        set_status(win, "File password was changed");
    }
    Ok(())
}

fn cb_copy_name(win: &PSMainWindow) -> Result<()> {
    if let Some((iter, _path)) = win.private().view.get_selected_iter() {
        if let Some(record) = win.private().data.borrow().get(&iter) {
            if let Some(username) = record.username() {
                get_clipboard().set_text(&username);
                set_status(win, "Name was copied to clipboard");
            }
        }
    }
    Ok(())
}

fn cb_copy_password(win: &PSMainWindow) -> Result<()> {
    if let Some((iter, _path)) = win.private().view.get_selected_iter() {
        if let Some(record) = win.private().data.borrow().get(&iter) {
            if let Some(password) = record.password() {
                get_clipboard().set_text(&password);
                set_status(win, "Secret (password) was copied to clipboard");
            }
        }
    }
    Ok(())
}

fn set_mode(win: &PSMainWindow, mode: AppMode) {
    match mode {
        AppMode::Initial => {
            for (action_name, action) in win.private().actions.borrow().iter() {
                let enabled = match action_name.group() {
                    PSActionGroup::Doc => false,
                    PSActionGroup::ViewMode => false,
                    PSActionGroup::MergeMode => false,
                    PSActionGroup::Record => false,
                };
                action.set_enabled(enabled);
            }
            if let Some(action) = win
                .private()
                .actions
                .borrow()
                .get(&PSAction::Doc(DocAction::MergeMode))
            {
                action.set_state(&false.into());
            }
            win.private().search_entry.set_sensitive(false);
            win.private().view.set_selection_mode(false);
            win.private().stack.set_visible_child_name("dashboard");
            win.private().dashboard.update();
        }
        AppMode::FileOpened => {
            for (action_name, action) in win.private().actions.borrow().iter() {
                let enabled = match action_name.group() {
                    PSActionGroup::Doc => true,
                    PSActionGroup::ViewMode => true,
                    PSActionGroup::MergeMode => false,
                    PSActionGroup::Record => true,
                };
                action.set_enabled(enabled);
            }
            if let Some(action) = win
                .private()
                .actions
                .borrow()
                .get(&PSAction::Doc(DocAction::MergeMode))
            {
                action.set_state(&false.into());
            }
            win.private().search_entry.set_sensitive(true);
            win.private().view.set_selection_mode(false);
            win.private().stack.set_visible_child_name("file");
        }
        AppMode::MergeMode => {
            for (action_name, action) in win.private().actions.borrow().iter() {
                let enabled = match action_name.group() {
                    PSActionGroup::Doc => true,
                    PSActionGroup::ViewMode => false,
                    PSActionGroup::MergeMode => true,
                    PSActionGroup::Record => false,
                };
                action.set_enabled(enabled);
            }
            if let Some(action) = win
                .private()
                .actions
                .borrow()
                .get(&PSAction::Doc(DocAction::MergeMode))
            {
                action.set_state(&true.into());
            }
            win.private().search_entry.set_sensitive(true);
            win.private().view.set_selection_mode(true);
            win.private().stack.set_visible_child_name("file");
        }
    }
    win.private().mode.set(mode);
}

fn set_merge_mode(win: &PSMainWindow, merge: bool) -> Result<()> {
    set_mode(
        win,
        if merge {
            AppMode::MergeMode
        } else {
            AppMode::FileOpened
        },
    );
    Ok(())
}

fn create_file_widget(tree_view: &PSTreeView, preview: &PSPreviewPanel) -> gtk::Widget {
    let paned = gtk::PanedBuilder::new()
        .orientation(gtk::Orientation::Horizontal)
        .hexpand(true)
        .vexpand(true)
        .build();

    let sw = gtk::ScrolledWindowBuilder::new()
        .can_focus(true)
        .hscrollbar_policy(gtk::PolicyType::Automatic)
        .vscrollbar_policy(gtk::PolicyType::Automatic)
        .shadow_type(gtk::ShadowType::In)
        .build();
    sw.add(&tree_view.get_widget());

    paned.pack1(&sw, true, false);

    paned.pack2(&preview.get_widget(), false, false);

    paned.upcast()
}

fn create_content_widget(dashboard: &gtk::Widget, file_widget: &gtk::Widget) -> gtk::Stack {
    let stack = gtk::Stack::new();
    stack.add_named(dashboard, "dashboard");
    stack.add_named(file_widget, "file");
    stack
}

fn create_main_window(
    gtk_app: &gtk::Application,
    content: &gtk::Widget,
) -> (gtk::ApplicationWindow, PSSearchEntry, gtk::Statusbar) {
    gtk_app.set_menubar(Some(&ui::menu::create_menu_bar()));

    let main_window = gtk::ApplicationWindowBuilder::new()
        .application(gtk_app)
        .title(WINDOW_TITLE)
        .icon_name("password-storage")
        .window_position(gtk::WindowPosition::Center)
        .default_width(1000)
        .default_height(800)
        .build();

    let search_entry = PSSearchEntry::new();

    let statusbar = gtk::Statusbar::new();

    let grid = {
        let grid = gtk::Grid::new();

        let toolbar = ui::toolbar::create_tool_bar(&search_entry.get_widget());
        toolbar.set_halign(gtk::Align::Fill);
        toolbar.set_valign(gtk::Align::Start);
        grid.attach(&toolbar, 0, 0, 1, 1);

        grid.attach(content, 0, 1, 1, 1);

        statusbar.set_halign(gtk::Align::Fill);
        statusbar.set_valign(gtk::Align::End);
        grid.attach(&statusbar, 0, 2, 1, 1);

        grid
    };

    main_window.add(&grid);

    (main_window, search_entry, statusbar)
}

fn create_action(
    win: &PSMainWindow,
    ps_action_name: actions::PSAction,
    cb: Box<dyn Fn(&PSMainWindow) -> Result<()>>,
) {
    let (_action_group_name, action_name) = ps_action_name.name();
    let action = gio::SimpleAction::new(&action_name, None);
    let win1 = win.clone();
    action.connect_activate(move |_, _| {
        if let Err(error) = cb(&win1) {
            say_error(&win1.window(), &error.to_string());
        }
    });
    win.private()
        .actions
        .borrow_mut()
        .insert(ps_action_name, action);
}

fn create_toggle_action(
    win: &PSMainWindow,
    ps_action_name: actions::PSAction,
    cb: Box<dyn Fn(&PSMainWindow, bool) -> Result<()>>,
) {
    let (_action_group_name, action_name) = ps_action_name.name();
    let action = gio::SimpleAction::new_stateful(&action_name, None, &false.into());
    let win1 = win.clone();
    action.connect_activate(move |action, _| {
        let prev_state: bool = action
            .get_state()
            .and_then(|state| state.get())
            .unwrap_or(false);
        let new_state: bool = !prev_state;
        action.set_state(&new_state.into());
        if let Err(error) = cb(&win1, new_state) {
            let window = win1.window();
            say_error(&window, &error.to_string());
        }
    });
    win.private()
        .actions
        .borrow_mut()
        .insert(ps_action_name, action);
}

pub fn old_main(
    app1: &gtk::Application,
    config: &Rc<RefCell<Config>>,
    cache: &Cache,
) -> PSMainWindow {
    let data = PSStore::new();
    let view = PSTreeView::new();
    let preview = PSPreviewPanel::new();

    let dashboard = PSDashboard::new(cache);

    let file_widget = create_file_widget(&view, &preview);

    let stack = create_content_widget(&dashboard.get_widget(), &file_widget);

    let (main_window, search_entry, statusbar) = create_main_window(app1, &stack.clone().upcast());

    view.set_model(&data.as_model());

    let private = PSMainWindowPrivate {
        mode: Cell::new(AppMode::Initial),
        stack,
        dashboard,
        data: RefCell::new(data),
        view,
        search_entry,
        preview,
        statusbar,

        actions: RefCell::new(HashMap::new()),

        filename: RefCell::new(None),
        password: RefCell::new(None),
        changed: Cell::new(false),

        config: config.clone(),
        cache: cache.clone(),
    };
    unsafe {
        main_window.set_data("private", private);
        main_window.set_data("is_main_window", true);
    }
    let win = PSMainWindow(main_window);

    win.0.connect_delete_event(
        clone!(@weak win => @default-return Inhibit(false), move |_win, _event| {
            if win.ensure_data_is_saved() {
                get_clipboard().clear();
                gtk::Inhibit(false)
            } else {
                gtk::Inhibit(true)
            }
        }),
    );

    {
        create_toggle_action(
            &win,
            PSAction::Doc(DocAction::MergeMode),
            Box::new(set_merge_mode),
        );

        create_action(
            &win,
            PSAction::ViewMode(ViewModeAction::MergeFile),
            Box::new(cb_merge_file),
        );
        create_action(
            &win,
            PSAction::ViewMode(ViewModeAction::Save),
            Box::new(cb_save),
        );
        create_action(
            &win,
            PSAction::ViewMode(ViewModeAction::SaveAs),
            Box::new(cb_save_as),
        );
        create_action(
            &win,
            PSAction::ViewMode(ViewModeAction::Close),
            Box::new(cb_close),
        );
        create_action(
            &win,
            PSAction::ViewMode(ViewModeAction::Find),
            Box::new(|win| {
                win.private().search_entry.grab_focus();
                Ok(())
            }),
        );
        create_action(
            &win,
            PSAction::ViewMode(ViewModeAction::ChangePassword),
            Box::new(cb_change_password),
        );

        for record_type in RECORD_TYPES.iter() {
            create_action(
                &win,
                PSAction::ViewMode(ViewModeAction::Add(record_type.name.to_string())),
                Box::new(move |app| cb_add_record(app, record_type)),
            );
        }

        create_action(
            &win,
            PSAction::MergeMode(MergeModeAction::UncheckAll),
            Box::new(cb_uncheck_all),
        );
        create_action(
            &win,
            PSAction::MergeMode(MergeModeAction::Merge),
            Box::new(cb_merge),
        );

        create_action(
            &win,
            PSAction::Record(RecordAction::CopyName),
            Box::new(cb_copy_name),
        );
        create_action(
            &win,
            PSAction::Record(RecordAction::CopyPassword),
            Box::new(cb_copy_password),
        );
        create_action(
            &win,
            PSAction::Record(RecordAction::Edit),
            Box::new(|win| {
                cb_edit_record(win);
                Ok(())
            }),
        );
        create_action(
            &win,
            PSAction::Record(RecordAction::Delete),
            Box::new(cb_delele_record),
        );

        for record_type in RECORD_TYPES.iter() {
            if !record_type.is_group {
                create_action(
                    &win,
                    PSAction::Record(RecordAction::ConvertTo(record_type.name.to_string())),
                    Box::new(move |app| cb_convert_record(app, record_type)),
                );
            }
        }
    }

    {
        let mut groups = HashMap::new();
        for (name, action) in win.private().actions.borrow().iter() {
            groups
                .entry(name.group())
                .or_insert_with(SimpleActionGroup::new)
                .add_action(action);
        }

        for (group_name, group) in &groups {
            win.0.insert_action_group(group_name.name(), Some(group));
        }
    }

    win.private()
        .dashboard
        .connect_activate(clone!(@weak win => move |filename| {
            do_open_file(&win, filename);
        }));

    win.private()
        .search_entry
        .connect_changed(clone!(@weak win => move |text| {
            if let Some(search_text) = text {
                let look_at_secrets = win.private().config.borrow().search_in_secrets;
                set_status(&win, "View is filtered.");
                let model = create_model_filter(
                    &win.private().data.borrow(),
                    &search_text,
                    look_at_secrets,
                );
                win.private().view.set_model(&model.upcast());
                win.private().view.view.set_reorderable(false);
                win.refilter();
                win.private().view.view.expand_all();
            } else {
                set_status(&win, "View filter was reset.");
                let model = win.private().data.borrow().as_model();
                win.private().view.set_model(&model);
                win.private().view.view.set_reorderable(true);
                win.private().view.view.collapse_all();
            }
            listview_cursor_changed(&win, None);
        }));

    win.private()
        .view
        .connect_cursor_changed(clone!(@weak win => move |selection| {
            let mut record = None;
            if let Some((iter, _path)) = selection {
                record = win.private().data.borrow().get(&iter);
            }
            listview_cursor_changed(&win, record);
        }));

    win.private()
        .view
        .connect_drop(clone!(@weak win => @default-return false, move |iter| {
            let record_opt = win.private().data.borrow().get(&iter);
            if let Some(record) = record_opt {
                record.record_type.is_group
            } else {
                false
            }
        }));

    win.private()
        .view
        .connect_row_activated(clone!(@weak win => move |selection| {
            if let Some((iter, path)) = selection {
                let record_opt = win.private().data.borrow().get(&iter);
                if let Some(record) = record_opt {
                    if record.record_type.is_group {
                        win.private().view.toggle_group(&path);
                    } else {
                        cb_edit_record(&win);
                    }
                }
            }
        }));

    let popup = ui::menu::create_tree_popup();
    win.private().view.set_popup(&popup);

    win.window().show_all();

    set_mode(&win, AppMode::Initial);

    win
}
