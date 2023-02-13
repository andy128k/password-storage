use crate::gtk_prelude::*;
use crate::model::record::{FIELD_NAME, RECORD_TYPE_GENERIC};
use crate::model::tree::{RecordNode, RecordTree};
use crate::utils::list_model::ListModelImmutableExt;
use crate::utils::typed_list_store::TypedListStore;
use crate::utils::ui::{scrolled, PSWidgetLookupExt};

pub fn singleton_list(item: &impl glib::IsA<glib::Object>) -> gio::ListStore {
    let list = gio::ListStore::new(item.type_());
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
        let item = obj.clone().downcast::<gio::ListStore>().ok()?;

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
        .selected_item()?
        .downcast::<gtk::TreeListRow>()
        .ok()?
        .item()?
        .downcast::<gio::ListStore>()
        .ok()?;
    Some(TypedListStore::from_untyped(path.sliced(1..)))
}

fn name_factory() -> gtk::SignalListItemFactory {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(|_factory, list_item| {
        let b = gtk::Box::new(gtk::Orientation::Horizontal, 8);

        let image = gtk::Image::from_icon_name("folder");
        b.append(&image);

        let label = gtk::Label::new(None);
        b.append(&label);

        let expander = gtk::TreeExpander::new();
        expander.set_child(Some(&b));

        list_item.set_child(Some(&expander));
    });
    factory.connect_bind(|_factory, list_item| {
        let Some(expander) = list_item.child().and_then(|child| child.downcast::<gtk::TreeExpander>().ok()) else { return };

        let row = list_item.item().and_then(|i| i.downcast::<gtk::TreeListRow>().ok());
        expander.set_list_row(row.as_ref());

        let record = row
            .and_then(|row| row.item())
            .and_then(|item| item.downcast_ref::<gio::ListModel>()?.last()?.downcast::<RecordNode>().ok());

        if let Some(label) = expander.child().and_then(|c| c.of_type::<gtk::Label>()) {
            label.set_text(&record.map(|r| r.record().name()).unwrap_or_else(|| "/".to_string()));
        }
    });
    factory.connect_unbind(|_factory, list_item| {
        let Some(expander) = list_item.child().and_then(|child| child.downcast::<gtk::TreeExpander>().ok()) else { return };
        if let Some(label) = expander.child().and_then(|c| c.of_type::<gtk::Label>()) {
            label.set_text("");
        }
    });
    factory.connect_teardown(|_factory, list_item| {
        let Some(expander) = list_item.child().and_then(|child| child.downcast::<gtk::TreeExpander>().ok()) else { return };
        if let Some(label) = expander.child().and_then(|c| c.of_type::<gtk::Label>()) {
            label.set_text("");
        }
    });
    factory
}

fn description_factory() -> gtk::SignalListItemFactory {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(|_factory, list_item| {
        let child = gtk::Label::builder().halign(gtk::Align::Start).build();
        list_item.set_child(Some(&child));
    });
    factory.connect_bind(|_factory, list_item| {
        let Some(label) = list_item.child().and_then(|child| child.downcast::<gtk::Label>().ok()) else { return };
        let record = list_item.item()
            .and_then(|i| i.downcast::<gtk::TreeListRow>().ok())
            .and_then(|row| row.item())
            .and_then(|item| item.downcast_ref::<gio::ListModel>()?.last()?.downcast::<RecordNode>().ok());

        label.set_text(&record.and_then(|r| r.record().description().lines().next().map(|s| s.to_string())).unwrap_or_default());
    });
    factory.connect_unbind(|_factory, list_item| {
        let Some(label) = list_item.child().and_then(|child| child.downcast::<gtk::Label>().ok()) else { return };
        label.set_text("");
    });
    factory.connect_teardown(|_factory, list_item| {
        let Some(label) = list_item.child().and_then(|child| child.downcast::<gtk::Label>().ok()) else { return };
        label.set_text("");
    });
    factory
}

pub async fn select_group(
    parent_window: &gtk::Window,
    title: &str,
    tree: &RecordTree,
) -> Option<TypedListStore<RecordNode>> {
    let dlg = gtk::Dialog::builder()
        .transient_for(parent_window)
        .use_header_bar(1)
        .modal(true)
        .resizable(true)
        .title(title)
        .icon_name("password-storage")
        .build();
    dlg.add_button("_Cancel", gtk::ResponseType::Cancel);
    dlg.add_button("_Ok", gtk::ResponseType::Ok);
    dlg.set_default_response(gtk::ResponseType::Ok);

    let model = make_groups_model(tree);

    let selection_model = gtk::SingleSelection::new(Some(model));

    let tree_view = gtk::ColumnView::builder()
        .can_focus(true)
        .model(&selection_model)
        .build();

    tree_view.append_column(
        &gtk::ColumnViewColumn::builder()
            .title("Name")
            .factory(&name_factory())
            .build(),
    );

    tree_view.append_column(
        &gtk::ColumnViewColumn::builder()
            .title("Description")
            .factory(&description_factory())
            .expand(true)
            .build(),
    );

    let scrolled_window = scrolled(&tree_view);
    scrolled_window.set_size_request(500, 400);

    dlg.content_area().set_margin_start(8);
    dlg.content_area().set_margin_end(8);
    dlg.content_area().set_margin_top(8);
    dlg.content_area().set_margin_bottom(8);
    dlg.content_area().append(&scrolled_window);

    dlg.set_response_sensitive(gtk::ResponseType::Ok, false);
    selection_model.connect_selection_changed(clone!(@weak dlg => move |selection_model, _, _| {
        dlg.set_response_sensitive(gtk::ResponseType::Ok, get_selected_record(selection_model).is_some());
    }));

    let answer = dlg.run_future().await;
    dlg.close();
    if answer == gtk::ResponseType::Ok {
        get_selected_record(&selection_model)
    } else {
        None
    }
}
