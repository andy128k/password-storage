pub async fn say(parent_window: &gtk::Window, message: &str) {
    if let Err(err) = gtk::AlertDialog::builder()
        .modal(true)
        .buttons(["Ok"])
        .message(message)
        .build()
        .choose_future(Some(parent_window))
        .await
    {
        eprintln!("Failed to say '{}': {}", message, err);
    }
}
