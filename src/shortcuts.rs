use crate::compat::accel::PRIMARY_MODIFIER_NAME;
use crate::primary_accel;

pub struct Shortcut<'a> {
    pub action: Option<&'a str>,
    pub title: &'a str,
    pub accel: &'a str,
    pub display_accel: &'a [&'a str],
}

pub type ShortcutGroup<'a> = (&'a str, &'a [Shortcut<'a>]);

pub const GENERAL_SHORTCUTS: &[Shortcut<'static>] = &[
    Shortcut {
        action: Some("app.new"),
        title: "New file",
        accel: primary_accel!("n"),
        display_accel: &[PRIMARY_MODIFIER_NAME, "N"],
    },
    Shortcut {
        action: Some("app.open"),
        title: "Open file",
        accel: primary_accel!("o"),
        display_accel: &[PRIMARY_MODIFIER_NAME, "O"],
    },
    Shortcut {
        action: Some("file.save"),
        title: "Save file",
        accel: primary_accel!("s"),
        display_accel: &[PRIMARY_MODIFIER_NAME, "S"],
    },
    Shortcut {
        action: Some("file.close"),
        title: "Close file",
        accel: primary_accel!("w"),
        display_accel: &[PRIMARY_MODIFIER_NAME, "W"],
    },
    Shortcut {
        action: Some("app.quit"),
        title: "Quit",
        accel: primary_accel!("q"),
        display_accel: &[PRIMARY_MODIFIER_NAME, "Q"],
    },
];

pub const DOCUMENT_SHORTCUTS: &[Shortcut<'static>] = &[
    Shortcut {
        action: Some("file.find"),
        title: "Find",
        accel: primary_accel!("f"),
        display_accel: &[PRIMARY_MODIFIER_NAME, "F"],
    },
    Shortcut {
        action: None,
        title: "Find next match",
        accel: primary_accel!("g"),
        display_accel: &[PRIMARY_MODIFIER_NAME, "G"],
    },
    Shortcut {
        action: None,
        title: "Find previous match",
        accel: primary_accel!("<Shift>g"),
        display_accel: &["Shift", PRIMARY_MODIFIER_NAME, "G"],
    },
    Shortcut {
        action: Some("entry.copy-name"),
        title: "Copy name",
        accel: primary_accel!("c"),
        display_accel: &[PRIMARY_MODIFIER_NAME, "C"],
    },
    Shortcut {
        action: Some("entry.copy-password"),
        title: "Copy password",
        accel: primary_accel!("<Shift>c"),
        display_accel: &["Shift", PRIMARY_MODIFIER_NAME, "C"],
    },
];

pub const MISC_SHORTCUTS: &[Shortcut<'static>] = &[Shortcut {
    action: Some("app.shortcuts"),
    title: "Keyboard shortcuts memo",
    accel: primary_accel!("question"),
    display_accel: &[PRIMARY_MODIFIER_NAME, "?"],
}];

pub const SHORTCUTS: &[ShortcutGroup<'static>] = &[
    ("General", GENERAL_SHORTCUTS),
    ("Document", DOCUMENT_SHORTCUTS),
    ("Miscellaneous", MISC_SHORTCUTS),
];
