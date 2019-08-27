use gtk::prelude::*;
use gtk::{Window, Widget, Grid, CheckButton};
use crate::ui::form::base::FormWidget;
use crate::ui::edit_object::edit_object;
use crate::config::Config;

struct ConfigForm {
    grid: Grid,
    search_in_secrets: CheckButton,
    show_secrets_on_preview: CheckButton,
}

impl ConfigForm {
    fn new() -> Self {
        let grid = Grid::new();
        grid.set_column_spacing(8);
        grid.set_row_spacing(8);

        let search_in_secrets = {
            let button = CheckButton::new_with_label("Search in secrets (passwords)");
            button.set_can_focus(true);
            grid.attach(&button, 0, 0, 2, 1);
            button
        };

        let show_secrets_on_preview = {
            let button = CheckButton::new_with_label("Show secrets (passwords) on preview panel");
            button.set_can_focus(true);
            grid.attach(&button, 0, 1, 2, 1);
            button
        };

        Self { grid, search_in_secrets, show_secrets_on_preview }
    }
}

impl FormWidget<Config> for ConfigForm {
    fn get_widget(&self) -> Widget {
        self.grid.clone().upcast()
    }

    fn get_value(&self) -> Option<Config> {
        Some(Config {
            search_in_secrets: self.search_in_secrets.get_active(),
            show_secrets_on_preview: self.show_secrets_on_preview.get_active()
        })
    }

    fn set_value(&self, value: Option<&Config>) {
        match value {
            Some(config) => {
                self.search_in_secrets.set_active(config.search_in_secrets);
                self.show_secrets_on_preview.set_active(config.show_secrets_on_preview);
            },
            None => {
                self.search_in_secrets.set_active(false);
                self.show_secrets_on_preview.set_active(false);
            }
        };
    }

    fn connect_changed(&mut self, _callback: Box<dyn Fn(Option<&Config>)>) {
        // unused
    }
}

pub fn preferences(parent_window: &Window, config: &Config) -> Option<Config> {
    let form = ConfigForm::new();
    edit_object(Some(config), form, parent_window, "Preferences", "password-storage")
}
