mod application;
mod ui;
mod utils;
mod weak_map;

macro_rules! run_test {
    ($name:path) => {{
        eprint!("test {} ... ", stringify!($name));
        $name();
        eprintln!("ok");
    }};
}

fn main() {
    eprintln!("");
    gtk4::init().unwrap();

    run_test!(weak_map::test_weak_map);
    run_test!(weak_map::test_weak_map_remove);

    run_test!(ui::forms::entry::test_text);
    run_test!(ui::forms::entry::test_text_value);
    run_test!(ui::forms::entry::test_text_event);
    run_test!(ui::forms::entry::test_name);
    run_test!(ui::forms::entry::test_name_value);
    run_test!(ui::forms::entry::test_name_event);
    run_test!(ui::forms::entry::test_password);
    run_test!(ui::forms::entry::test_password_value);
    run_test!(ui::forms::entry::test_password_event);

    run_test!(ui::forms::multiline::test_multiline);
    run_test!(ui::forms::multiline::test_multiline_value);
    run_test!(ui::forms::multiline::test_multiline_event);

    run_test!(ui::forms::form::test_form);
    run_test!(ui::forms::form::test_form_value);
    run_test!(ui::forms::form::test_form_event);

    run_test!(ui::password_editor::test_open_password);
    run_test!(ui::password_editor::test_open_password_value);
    run_test!(ui::password_editor::test_open_password_event);

    run_test!(utils::list_model::test_last);
    run_test!(utils::list_model::test_slice);
    run_test!(utils::list_model::test_appended);

    run_test!(utils::ui::test_bitset_iter);

    run_test!(application::test_app_create);

    eprintln!("");
}
