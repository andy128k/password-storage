error_chain!{
    errors {
        BadFile {
            description("Bad file")
            display("Bad file")
        }
        Crypto(err: ::crypto::symmetriccipher::SymmetricCipherError) {
            description("Crypto error")
            display("Crypto error: {:?}", err)
        }
    }
    links {
        MiniDom(::minidom::Error, ::minidom::ErrorKind);
    }
    foreign_links {
        Utf8(::std::string::FromUtf8Error);
        Io(::std::io::Error);
        Glib(::glib::Error);
        TomlSer(::toml::ser::Error);
        TomlDe(::toml::de::Error);
    }
}
