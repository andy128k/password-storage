use std::io::{Error, ErrorKind, Read, Result, Write};

pub struct FileHeader {
    pub data_version: u8,
    pub app_version: (u8, u8, u8),
}

impl FileHeader {
    const MAGIC: [u8; 4] = *b"rvl\0";

    pub fn read(source: &mut dyn Read) -> Result<Self> {
        #[inline]
        fn read_byte(source: &mut dyn Read) -> Result<u8> {
            let mut buffer: [u8; 1] = [0; 1];
            source.read_exact(&mut buffer)?;
            Ok(buffer[0])
        }

        #[inline]
        fn expect_byte(source: &mut dyn Read, expected: u8) -> Result<()> {
            let byte = read_byte(source)?;
            if byte == expected {
                Ok(())
            } else {
                Err(Error::new(
                    ErrorKind::Other,
                    format!("Unexpected byte {} ({} was expected).", byte, expected),
                ))
            }
        }

        #[inline]
        fn expect_bytes(source: &mut dyn Read, expected: &[u8]) -> Result<()> {
            for e in expected {
                expect_byte(source, *e)?;
            }
            Ok(())
        }

        expect_bytes(source, &Self::MAGIC)?;
        let data_version = read_byte(source)?;
        expect_byte(source, 0)?;
        let app_version_major = read_byte(source)?;
        let app_version_minor = read_byte(source)?;
        let app_version_patch = read_byte(source)?;
        expect_bytes(source, b"\0\0\0")?;

        Ok(Self {
            data_version,
            app_version: (app_version_major, app_version_minor, app_version_patch),
        })
    }

    pub fn write(&self, writer: &mut dyn Write) -> Result<()> {
        #[inline]
        fn write_byte(writer: &mut dyn Write, value: u8) -> Result<()> {
            writer.write_all(&value.to_be_bytes())
        }

        writer.write_all(&Self::MAGIC)?;
        write_byte(writer, self.data_version)?;
        write_byte(writer, 0)?;
        write_byte(writer, self.app_version.0)?;
        write_byte(writer, self.app_version.1)?;
        write_byte(writer, self.app_version.2)?;
        writer.write_all(b"\0\0\0")?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_header_read() {
        let data = [b'r', b'v', b'l', 0, 200, 0, 100, 101, 102, 0, 0, 0];
        let header = FileHeader::read(&mut Cursor::new(&data)).unwrap();
        assert_eq!(header.data_version, 200);
        assert_eq!(header.app_version, (100, 101, 102));
    }

    #[test]
    fn test_header_write() {
        let mut buf = Vec::new();
        let header = FileHeader {
            data_version: 1,
            app_version: (0, 4, 11),
        };
        header.write(&mut buf).unwrap();
        assert_eq!(buf, &[b'r', b'v', b'l', 0, 1, 0, 0, 4, 11, 0, 0, 0]);
    }
}
