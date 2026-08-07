use async_channel::Sender;

pub trait SenderExt<T> {
    fn toss(&self, message: T);
}

impl<T> SenderExt<T> for Sender<T> {
    fn toss(&self, message: T) {
        if let Err(error) = self.try_send(message) {
            eprintln!("{}", error);
        }
    }
}
