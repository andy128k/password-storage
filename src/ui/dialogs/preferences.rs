use crate::config::Config;
use crate::gtk_prelude::*;
use crate::ui::edit_object::edit_object;
use crate::ui::forms::base::FormWidget;

struct ConfigForm {
    grid: gtk::Grid,
    search_in_secrets: gtk::CheckButton,
    show_secrets_on_preview: gtk::CheckButton,
}

impl ConfigForm {
    fn new() -> Self {
        let grid = gtk::Grid::new();
        grid.set_column_spacing(8);
        grid.set_row_spacing(8);

        let search_in_secrets = gtk::CheckButton::builder()
            .label("Search in secrets (passwords)")
            .can_focus(true)
            .build();
        grid.attach(&search_in_secrets, 0, 0, 2, 1);

        let show_secrets_on_preview = gtk::CheckButton::builder()
            .label("Show secrets (passwords) on preview panel")
            .can_focus(true)
            .build();
        grid.attach(&show_secrets_on_preview, 0, 1, 2, 1);

        Self {
            grid,
            search_in_secrets,
            show_secrets_on_preview,
        }
    }
}

impl FormWidget<Config> for ConfigForm {
    fn get_widget(&self) -> gtk::Widget {
        self.grid.clone().upcast()
    }

    fn get_value(&self) -> Option<Config> {
        Some(Config {
            search_in_secrets: self.search_in_secrets.is_active(),
            show_secrets_on_preview: self.show_secrets_on_preview.is_active(),
        })
    }

    fn set_value(&self, value: Option<&Config>) {
        match value {
            Some(config) => {
                self.search_in_secrets.set_active(config.search_in_secrets);
                self.show_secrets_on_preview
                    .set_active(config.show_secrets_on_preview);
            }
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

pub async fn preferences(parent_window: &gtk::Window, config: &Config) -> Option<Config> {
    let form = ConfigForm::new();
    edit_object(
        Some(config),
        form,
        parent_window,
        "Preferences",
        "password-storage",
    )
    .await
}
