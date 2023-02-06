use once_cell::sync::Lazy;
use std::fmt;
use std::str::FromStr;

#[derive(PartialEq, Eq, Default, Debug, Clone, Copy)]
pub struct Version {
    pub major: u8,
    pub minor: u8,
    pub patch: u8,
}

impl FromStr for Version {
    type Err = String;

    fn from_str(version: &str) -> Result<Self, Self::Err> {
        let parts = version
            .split('.')
            .map(|p| p.parse())
            .collect::<Result<Vec<u8>, _>>()
            .map_err(|err| err.to_string())?;
        if parts.len() != 3 {
            return Err(format!(
                "Bad version '{version}'. Three components are expected."
            ));
        }
        Ok(Version {
            major: parts[0],
            minor: parts[1],
            patch: parts[2],
        })
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub static VERSION_PARSED: Lazy<Version> = Lazy::new(|| VERSION.parse().unwrap());

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_version() {
        assert_eq!(
            Version::from_str("0.1.2"),
            Ok(Version {
                major: 0,
                minor: 1,
                patch: 2
            })
        );
        assert_eq!(
            Version::from_str("0.0.0"),
            Ok(Version {
                major: 0,
                minor: 0,
                patch: 0
            })
        );
        assert_eq!(
            Version::from_str("2.43.443"),
            Err("number too large to fit in target type".to_string())
        );
    }

    #[test]
    fn test_crate_version() {
        assert_eq!(VERSION_PARSED.to_string(), VERSION);
    }
}
