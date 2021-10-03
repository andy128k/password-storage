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

type ShortcutGroup<'a> = (&'a str, &'a [Shortcut<'a>]);

pub const SHORTCUTS: &[ShortcutGroup<'static>] = &[
    (
        "General",
        &[
            Shortcut::new("<Primary>n", "New file").action("app.new"),
            Shortcut::new("<Primary>o", "Open file").action("app.open"),
            Shortcut::new("<Primary>s", "Save file").action("file.save"),
            Shortcut::new("<Primary>w", "Close file").action("file.close"),
            Shortcut::new("<Primary>q", "Quit").action("app.quit"),
        ],
    ),
    (
        "Document",
        &[
            Shortcut::new("<Primary>f", "Find").action("file.find"),
            Shortcut::new("<Primary>g", "Find next match"),
            Shortcut::new("<Primary><Shift>g", "Find previous match"),
            Shortcut::new("<Primary>c", "Copy name").action("entry.copy-name"),
            Shortcut::new("<Primary><Shift>c", "Copy password").action("entry.copy-password"),
        ],
    ),
    (
        "Miscellaneous",
        &[Shortcut::new("<Primary>question", "Keyboard shortcuts memo").action("app.shortcuts")],
    ),
];
