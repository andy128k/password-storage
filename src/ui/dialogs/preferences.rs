use std::path::Path;
use gtk::prelude::*;
use gtk::{Window, Widget, Label, Grid, FileChooserButton, FileChooserAction, CheckButton};
use ui::form::base::FormWidget;
use ui::edit_object::edit_object;
use config::Config;

struct ConfigForm {
    grid: Grid,
    default_file: FileChooserButton,
    search_in_secrets: CheckButton,
    show_secrets_on_preview: CheckButton,
}

impl ConfigForm {
    fn new() -> Self {
        let grid = Grid::new();
        grid.set_column_spacing(8);
        grid.set_row_spacing(8);

        let default_file = {
            let label_widget = Label::new("Default file");
            label_widget.set_xalign(0f32);
            label_widget.set_yalign(0.5f32);
            grid.attach(&label_widget, 0, 0, 1, 1);
            
            let button = FileChooserButton::new("", FileChooserAction::Open);
            grid.attach(&button, 1, 0, 1, 1);

            button
        };

        let search_in_secrets = {
            let button = CheckButton::new_with_label("Search in secrets (passwords)");
            button.set_can_focus(true);
            grid.attach(&button, 0, 1, 2, 1);
            button
        };

        let show_secrets_on_preview = {
            let button = CheckButton::new_with_label("Show secrets (passwords) on preview panel");
            button.set_can_focus(true);
            grid.attach(&button, 0, 2, 2, 1);
            button
        };

        Self { grid, default_file, search_in_secrets, show_secrets_on_preview }
    }
}

impl FormWidget<Config> for ConfigForm {
    fn get_widget(&self) -> Widget {
        self.grid.clone().upcast()
    }

    fn get_value(&self) -> Option<Config> {
        Some(Config {
            default_file: self.default_file.get_filename().and_then(|p| p.to_str().map(|s| s.to_string())),
            search_in_secrets: self.search_in_secrets.get_active(),
            show_secrets_on_preview: self.show_secrets_on_preview.get_active()
        })
    }

    fn set_value(&self, value: Option<&Config>) {
        match value {
            Some(config) => {
                if let Some(ref file) = config.default_file {
                    self.default_file.set_filename(Path::new(file));
                } else {
                    self.default_file.unselect_all();
                }
                self.search_in_secrets.set_active(config.search_in_secrets);
                self.show_secrets_on_preview.set_active(config.show_secrets_on_preview);
            },
            None => {
                self.default_file.unselect_all();
                self.search_in_secrets.set_active(false);
                self.show_secrets_on_preview.set_active(false);
            }
        };
    }

    fn connect_changed(&mut self, _callback: Box<Fn(Option<&Config>)>) {
        // unused
    }
}

pub fn preferences(parent_window: &Window, config: &Config) -> Option<Config> {
    let form = ConfigForm::new();
    edit_object(Some(config), form, parent_window, "Preferences", "password-storage")
}
