pub async fn show_uri(parent_window: Option<&gtk::Window>, uri: &str) {
    if let Err(err) = gtk::UriLauncher::builder()
        .uri(uri)
        .build()
        .launch_future(parent_window)
        .await
    {
        eprintln!("Failed to open a URI {}. {}", uri, err);
    }
}
