#[derive(PartialEq, Eq, Hash)]
pub enum AppAction {
    New,
    Open,
    Quit,
    Preferences,
    About
}

#[derive(PartialEq, Eq, Hash)]
pub enum DocAction {
    MergeMode
}

#[derive(PartialEq, Eq, Hash)]
pub enum ViewModeAction {
    Save,
    SaveAs,
    MergeFile,
    ChangePassword,
    Find,
    Add(String)
}

#[derive(PartialEq, Eq, Hash)]
pub enum MergeModeAction {
    UncheckAll,
    Merge
}

#[derive(PartialEq, Eq, Hash)]
pub enum RecordAction {
    CopyName,
    CopyPassword,
    Edit,
    Delete,
    ConvertTo(String)
}

#[derive(PartialEq, Eq, Hash)]
pub enum PSActionGroup {
    App,
    Doc,
    ViewMode,
    MergeMode,
    Record
}

#[derive(PartialEq, Eq, Hash)]
pub enum PSAction {
    App(AppAction),
    Doc(DocAction),
    ViewMode(ViewModeAction),
    MergeMode(MergeModeAction),
    Record(RecordAction)
}

// impl

impl AppAction {
    pub fn name(&self) -> String {
        match *self {
            AppAction::New => "new",
            AppAction::Open => "open",
            AppAction::Quit => "quit",
            AppAction::Preferences => "preferences",
            AppAction::About => "about"
        }.to_string()
    }
}

impl DocAction {
    pub fn name(&self) -> String {
        match *self {
            DocAction::MergeMode => "merge-mode"
        }.to_string()
    }
}

impl ViewModeAction {
    pub fn name(&self) -> String {
        match *self {
            ViewModeAction::Save => "save".to_string(),
            ViewModeAction::SaveAs => "save-as".to_string(),
            ViewModeAction::MergeFile => "merge-file".to_string(),
            ViewModeAction::ChangePassword => "change-password".to_string(),
            ViewModeAction::Find => "find".to_string(),
            ViewModeAction::Add(ref record_type) => format!("add-{}", record_type)
        }
    }
}

impl MergeModeAction {
    pub fn name(&self) -> String {
        match *self {
            MergeModeAction::UncheckAll => "uncheck-all",
            MergeModeAction::Merge => "merge"
        }.to_string()
    }
}

impl RecordAction {
    pub fn name(&self) -> String {
        match *self {
            RecordAction::CopyName => "copy-name".to_string(),
            RecordAction::CopyPassword => "copy-password".to_string(),
            RecordAction::Edit => "edit".to_string(),
            RecordAction::Delete => "delete".to_string(),
            RecordAction::ConvertTo(ref record_type) => format!("convert-to-{}", record_type)
        }
    }
}

impl PSActionGroup {
    pub fn name(&self) -> &str {
        match *self {
            PSActionGroup::App => "app",
            PSActionGroup::Doc => "doc",
            PSActionGroup::ViewMode => "file",
            PSActionGroup::MergeMode => "merge",
            PSActionGroup::Record => "entry"
        }
    }
}

impl PSAction {
    pub fn group(&self) -> PSActionGroup {
        match *self {
            PSAction::App(_) => PSActionGroup::App,
            PSAction::Doc(_) => PSActionGroup::Doc,
            PSAction::ViewMode(_) => PSActionGroup::ViewMode,
            PSAction::MergeMode(_) => PSActionGroup::MergeMode,
            PSAction::Record(_) => PSActionGroup::Record
        }
    }

    pub fn name(&self) -> (String, String) {
        let action_name = match *self {
            PSAction::App(ref action) => action.name(),
            PSAction::Doc(ref action) => action.name(),
            PSAction::ViewMode(ref action) => action.name(),
            PSAction::MergeMode(ref action) => action.name(),
            PSAction::Record(ref action) => action.name()
        };
        (self.group().name().to_string(), action_name)
    }

    pub fn full_name(&self) -> String {
        let action_name = self.name();
        format!("{}.{}", action_name.0, action_name.1)
    }
}
