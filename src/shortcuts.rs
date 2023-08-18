use crate::primary_accel;

pub struct Shortcut<'a> {
    pub action: Option<&'a str>,
    pub title: &'a str,
    pub accel: &'a str,
}

impl<'a> Shortcut<'a> {
    pub const fn new(accel: &'a str, title: &'a str) -> Self {
        Self {
            action: None,
            title,
            accel,
        }
    }

    pub const fn action(mut self, action: &'a str) -> Self {
        self.action = Some(action);
        self
    }
}

pub type ShortcutGroup<'a> = (&'a str, &'a [Shortcut<'a>]);

pub const SHORTCUTS: &[ShortcutGroup<'static>] = &[
    (
        "General",
        &[
            Shortcut::new(primary_accel!("n"), "New file").action("app.new"),
            Shortcut::new(primary_accel!("o"), "Open file").action("app.open"),
            Shortcut::new(primary_accel!("s"), "Save file").action("file.save"),
            Shortcut::new(primary_accel!("w"), "Close file").action("file.close"),
            Shortcut::new(primary_accel!("q"), "Quit").action("app.quit"),
        ],
    ),
    (
        "Document",
        &[
            Shortcut::new(primary_accel!("f"), "Find").action("file.find"),
            Shortcut::new(primary_accel!("g"), "Find next match"),
            Shortcut::new(primary_accel!("<Shift>g"), "Find previous match"),
            Shortcut::new(primary_accel!("c"), "Copy name").action("entry.copy-name"),
            Shortcut::new(primary_accel!("<Shift>c"), "Copy password")
                .action("entry.copy-password"),
        ],
    ),
    (
        "Miscellaneous",
        &[
            Shortcut::new(primary_accel!("question"), "Keyboard shortcuts memo")
                .action("app.shortcuts"),
        ],
    ),
];
