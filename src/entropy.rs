use std::cmp::Eq;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

pub trait CharClassifier<T> {
    fn classify(&self, ch: u8) -> T;
    fn class_size(&self, class: T) -> u32;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum AsciiCharClass {
    Control,
    Number,
    Upper,
    Lower,
    Punct1,
    Punct2,
    Extended,
}

#[derive(Default)]
pub struct AsciiClassifier;

impl CharClassifier<AsciiCharClass> for AsciiClassifier {
    fn classify(&self, ch: u8) -> AsciiCharClass {
        match ch {
            0..=31 | 127 => AsciiCharClass::Control,
            b'0'..=b'9' => AsciiCharClass::Number,
            b'A'..=b'Z' => AsciiCharClass::Upper,
            b'a'..=b'z' => AsciiCharClass::Lower,
            b' ' | b'!' | b'#' | b'$' | b'%' | b'&' | b'(' | b')' | b'*' | b',' | b'.' | b'/'
            | b'=' | b'@' | b'^' | b'_' => AsciiCharClass::Punct1,
            b'"' | b'\'' | b'+' | b'-' | b':' | b';' | b'<' | b'>' | b'?' | b'[' | b'\\' | b']'
            | b'`' | b'{' | b'|' | b'}' | b'~' => AsciiCharClass::Punct2,
            128..=255 => AsciiCharClass::Extended,
        }
    }

    fn class_size(&self, class: AsciiCharClass) -> u32 {
        match class {
            AsciiCharClass::Control => 33,
            AsciiCharClass::Number => 10,
            AsciiCharClass::Upper => 26,
            AsciiCharClass::Lower => 26,
            AsciiCharClass::Punct1 => 16,
            AsciiCharClass::Punct2 => 17,
            AsciiCharClass::Extended => 128,
        }
    }
}

#[derive(Default)]
struct Counter<T>(pub HashMap<T, u32>);

impl<T: Hash + Eq> Counter<T> {
    fn count(&mut self, value: T) -> u32 {
        let c = self.0.entry(value).or_insert(0);
        *c += 1;
        *c
    }
}

fn character_distance(ch1: u8, ch2: u8) -> i32 {
    // TODO: check in known sequences... qwertyuiop, PI digits...
    (i32::from(ch1) - i32::from(ch2)).abs()
}

pub fn password_entropy<T>(classifier: &dyn CharClassifier<T>, passw: &[u8]) -> f32
where
    T: Eq + std::hash::Hash,
{
    let mut classes = HashSet::new();
    let mut char_count = Counter::<u8>::default(); // to count characters quantities
    let mut distances = Counter::<i32>::default(); // to collect differences between adjacent characters

    let mut eff_len = 0_f32;
    let mut prev_nc: Option<u8> = None;

    for nc in passw {
        classes.insert(classifier.classify(*nc));

        // value/factor for increment effective length
        let dw = match prev_nc {
            None => 1,
            Some(pnc) => {
                let d = character_distance(*nc, pnc);
                distances.count(d)
            }
        };
        let cw = char_count.count(*nc);

        let dcw = (dw * cw) as f32;

        eff_len += 1.0 / dcw;
        prev_nc = Some(*nc);
    }

    // Password complexity index
    let mut pci = 0;
    for c in classes {
        pci += classifier.class_size(c);
    }

    if pci != 0 {
        (pci as f32).log2() * eff_len
    } else {
        0f32
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PasswordStrength {
    VeryWeak,
    Weak,
    Reasonable,
    Strong,
    VeryStrong,
}

impl PasswordStrength {
    pub fn display(self) -> &'static str {
        match self {
            PasswordStrength::VeryWeak => "Very weak",
            PasswordStrength::Weak => "Weak",
            PasswordStrength::Reasonable => "Reasonable",
            PasswordStrength::Strong => "Strong",
            PasswordStrength::VeryStrong => "Very strong",
        }
    }
}

impl From<f32> for PasswordStrength {
    fn from(entropy: f32) -> Self {
        if entropy < 28.0 {
            PasswordStrength::VeryWeak
        } else if entropy < 36.0 {
            PasswordStrength::Weak
        } else if entropy < 60.0 {
            PasswordStrength::Reasonable
        } else if entropy < 128.0 {
            PasswordStrength::Strong
        } else {
            PasswordStrength::VeryStrong
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ascii_classifier() {
        let classifier = AsciiClassifier;
        let mut map = HashMap::new();
        for ch in 0..=255 {
            let class = classifier.classify(ch);
            let class_chars = map.entry(class).or_insert_with(Vec::new);
            class_chars.push(ch);
        }
        for (class, chars) in &map {
            assert_eq!(
                chars.len() as u32,
                classifier.class_size(*class),
                "Size of {:?} class.",
                class
            );
        }
    }

    #[test]
    fn test_entropy_1() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrength = password_entropy(&classifier, b"1").into();
        assert_eq!(entropy, PasswordStrength::VeryWeak);
    }

    #[test]
    fn test_entropy_123456() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrength = password_entropy(&classifier, b"123456").into();
        assert_eq!(entropy, PasswordStrength::VeryWeak);
    }

    #[test]
    fn test_entropy_alphabet() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrength = password_entropy(&classifier, b"abcdef").into();
        assert_eq!(entropy, PasswordStrength::VeryWeak);
    }

    #[test]
    fn test_entropy_qwerty() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrength = password_entropy(&classifier, b"QWERTY").into();
        assert_eq!(entropy, PasswordStrength::Weak);
    }

    #[test]
    fn test_entropy_random_weak() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrength = password_entropy(&classifier, b"vsPi0v").into();
        assert_eq!(entropy, PasswordStrength::Weak);
    }

    #[test]
    fn test_entropy_random_reasonable() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrength = password_entropy(&classifier, b"vsPi0vQk").into();
        assert_eq!(entropy, PasswordStrength::Reasonable);
    }

    #[test]
    fn test_entropy_random_strong() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrength = password_entropy(&classifier, b"vsPi0vQk8J0KSHlG").into();
        assert_eq!(entropy, PasswordStrength::Strong);
    }
}
