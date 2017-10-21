pub trait ResultLogExt<T> {
    fn ok_log(self) -> Option<T>;
}

impl<T, E> ResultLogExt<T> for ::std::result::Result<T, E> where E: ::std::fmt::Debug {
    fn ok_log(self) -> Option<T> {
        match self {
            Ok(ok) => Some(ok),
            Err(err) => {
                println!("{:?}", err);
                None
            }
        }
    }
}
