pub enum AskSave {
    Discard,
    Cancel,
    Save,
}

pub async fn ask_save(parent_window: &gtk::Window, message: &str) -> AskSave {
    let answer = gtk::AlertDialog::builder()
        .modal(true)
        .buttons(["Discard changes", "Cancel", "Save"])
        .default_button(2)
        .cancel_button(1)
        .message(message)
        .build()
        .choose_future(Some(parent_window))
        .await;
    match answer {
        Ok(0) => AskSave::Discard,
        Ok(2) => AskSave::Save,
        _ => AskSave::Cancel,
    }
}
