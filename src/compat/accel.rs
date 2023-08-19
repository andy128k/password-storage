use gtk::gdk;

#[macro_export]
macro_rules! primary_accel {
    ($accel:expr) => {
        if cfg!(target_os = "macos") {
            concat!("<Meta>", $accel)
        } else {
            concat!("<Control>", $accel)
        }
    };
}

pub const PRIMARY_MODIFIER: gdk::ModifierType = if cfg!(target_os = "macos") {
    gdk::ModifierType::META_MASK
} else {
    gdk::ModifierType::CONTROL_MASK
};

pub const PRIMARY_MODIFIER_NAME: &str = if cfg!(target_os = "macos") {
    "\u{2318}"
} else {
    "Ctrl"
};
