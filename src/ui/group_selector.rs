use super::dialogs::generic_dialog::GenericDialog;
use crate::model::record::{FIELD_NAME, RECORD_TYPE_GENERIC};
use crate::model::tree::{RecordNode, RecordTree};
use crate::utils::list_model::ListModelImmutableExt;
use crate::utils::typed_list_store::TypedListStore;
use crate::utils::ui::{scrolled, PSWidgetLookupExt};
use gtk::{gio, glib, prelude::*};

use super::list_item_factory::PSListItemFactory;

pub fn singleton_list(item: &impl IsA<glib::Object>) -> gio::ListStore {
    let list = gio::ListStore::with_type(item.type_());
    list.append(item);
    list
}

fn fake_root(record_tree: &RecordTree) -> RecordNode {
    let mut root_record = RECORD_TYPE_GENERIC.new_record();
    root_record.set_field(&FIELD_NAME, "/");
    RecordNode::group(root_record, &record_tree.records)
}

fn make_groups_model(record_tree: &RecordTree) -> gtk::TreeListModel {
    let toplevel = singleton_list(&singleton_list(&fake_root(record_tree)));
    gtk::TreeListModel::new(toplevel, false, true, |obj| {
        let item = obj.downcast_ref::<gio::ListStore>()?;

        let children: TypedListStore<_> = item
            .last()?
            .downcast_ref::<RecordNode>()?
            .children()?
            .iter()
            .filter(|r| r.is_group())
            .map(|r| item.appended(&r))
            .collect();

        if children.is_empty() {
            None
        } else {
            Some(children.untyped().clone().upcast())
        }
    })
}

fn get_selected_record(
    selection_model: &gtk::SingleSelection,
) -> Option<TypedListStore<RecordNode>> {
    let path = selection_model
        .selected_item()
        .and_downcast::<gtk::TreeListRow>()?
        .item()
        .and_downcast::<gio::ListStore>()?;
    Some(TypedListStore::from_untyped(path.sliced(1..)))
}

struct NameFactory;

impl NameFactory {
    fn set_text(expander: &gtk::TreeExpander, text: &str) {
        if let Some(label) = expander.child().and_then(|c| c.of_type::<gtk::Label>()) {
            label.set_text(text);
        }
    }
}

impl PSListItemFactory for NameFactory {
    type Child = gtk::TreeExpander;

    fn setup(&self) -> gtk::TreeExpander {
        let b = gtk::Box::new(gtk::Orientation::Horizontal, 8);

        let image = gtk::Image::from_icon_name("folder");
        b.append(&image);

        let label = gtk::Label::new(None);
        b.append(&label);

        let expander = gtk::TreeExpander::new();
        expander.set_child(Some(&b));

        expander
    }

    fn bind(&self, list_item: &gtk::ListItem, expander: &gtk::TreeExpander) {
        let row = list_item.item().and_downcast::<gtk::TreeListRow>();
        expander.set_list_row(row.as_ref());

        let item = row
            .and_then(|row| row.item())
            .and_downcast::<gio::ListModel>();
        let record = item
            .and_then(|item| item.last())
            .and_downcast::<RecordNode>();

        Self::set_text(
            expander,
            &record
                .map(|r| r.record().name())
                .unwrap_or_else(|| "/".to_string()),
        );
    }

    fn unbind(&self, _list_item: &gtk::ListItem, expander: &gtk::TreeExpander) {
        Self::set_text(expander, "");
    }

    fn teardown(&self, _list_item: &gtk::ListItem, expander: &gtk::TreeExpander) {
        Self::set_text(expander, "");
    }
}

struct DescriptionFactory;

impl PSListItemFactory for DescriptionFactory {
    type Child = gtk::Label;

    fn setup(&self) -> gtk::Label {
        gtk::Label::builder().halign(gtk::Align::Start).build()
    }

    fn bind(&self, list_item: &gtk::ListItem, label: &gtk::Label) {
        let row = list_item.item().and_downcast::<gtk::TreeListRow>();
        let item = row
            .and_then(|row| row.item())
            .and_downcast::<gio::ListModel>();
        let record = item
            .and_then(|item| item.last())
            .and_downcast::<RecordNode>();

        label.set_text(
            &record
                .and_then(|r| {
                    r.record()
                        .description()
                        .lines()
                        .next()
                        .map(|s| s.to_string())
                })
                .unwrap_or_default(),
        );
    }

    fn unbind(&self, _list_item: &gtk::ListItem, label: &gtk::Label) {
        label.set_text("");
    }
}

pub async fn select_group(
    parent_window: &gtk::Window,
    window_title: &str,
    tree: &RecordTree,
) -> Option<TypedListStore<RecordNode>> {
    let dlg = GenericDialog::default();
    dlg.set_ok_label("Move");
    dlg.set_title(window_title);
    dlg.set_transient_for(Some(parent_window));

    let model = make_groups_model(tree);

    let selection_model = gtk::SingleSelection::new(Some(model));

    let tree_view = gtk::ColumnView::builder()
        .can_focus(true)
        .model(&selection_model)
        .build();

    tree_view.append_column(
        &gtk::ColumnViewColumn::builder()
            .title("Name")
            .factory(&NameFactory.into_factory())
            .build(),
    );

    tree_view.append_column(
        &gtk::ColumnViewColumn::builder()
            .title("Description")
            .factory(&DescriptionFactory.into_factory())
            .expand(true)
            .build(),
    );

    tree_view.connect_activate(
        glib::clone!(@weak dlg => move |_, _| dlg.emit_response(gtk::ResponseType::Ok)),
    );

    let scrolled_window = scrolled(&tree_view);
    scrolled_window.set_size_request(500, 400);
    dlg.set_child(Some(&scrolled_window));

    selection_model.connect_selection_changed(
        glib::clone!(@weak dlg => move |selection_model, _, _| {
            dlg.set_ok_sensitive(get_selected_record(selection_model).is_some());
        }),
    );

    match dlg.run().await {
        Some(gtk::ResponseType::Ok) => get_selected_record(&selection_model),
        _ => None,
    }
}
