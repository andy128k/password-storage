use std::collections::{HashMap, HashSet};
use std::convert::Into;

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
            48..=57 => AsciiCharClass::Number,
            65..=90 => AsciiCharClass::Upper,
            97..=122 => AsciiCharClass::Lower,
            32 | 33 | 35 | 36 | 37 | 38 | 40 | 41 | 42 | 43 | 45 | 47 | 61 | 64 | 94 | 95 => {
                AsciiCharClass::Punct1
            }
            34 | 39 | 44 | 46 | 58 | 59 | 60 | 62 | 63 | 91 | 92 | 93 | 96 | 123 | 124 | 125
            | 126 => AsciiCharClass::Punct2,
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

fn character_distance(ch1: u8, ch2: u8) -> i32 {
    // TODO: check in known sequences... qwertyuiop, PI digits...
    (i32::from(ch1) - i32::from(ch2)).abs()
}

pub fn password_entropy<T>(classifier: &dyn CharClassifier<T>, passw: &[u8]) -> f32
where
    T: Eq + std::hash::Hash,
{
    let mut classes = HashSet::new();
    let mut char_count = HashMap::<u8, u32>::new(); // to count characters quantities
    let mut distances = HashMap::<i32, u32>::new(); // to collect differences between adjacent characters

    let mut eff_len = 0f32;
    let mut prev_nc: Option<u8> = None;

    for nc in passw {
        classes.insert(classifier.classify(*nc));

        // value/factor for increment effective length
        let dw = match prev_nc {
            None => 1,
            Some(pnc) => {
                let d = character_distance(*nc, pnc);
                let ddw = distances.entry(d).or_insert(0);
                *ddw += 1;
                *ddw
            }
        };
        let cw = {
            let ccw = char_count.entry(*nc).or_insert(0);
            *ccw += 1;
            *ccw
        };

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

#[derive(Debug, PartialEq, Eq)]
pub enum PasswordStrenth {
    VeryWeak,
    Weak,
    Reasonable,
    Strong,
    VeryStrong,
}

impl Into<PasswordStrenth> for f32 {
    fn into(self) -> PasswordStrenth {
        if self < 28.0 {
            PasswordStrenth::VeryWeak
        } else if self >= 28.0 && self < 36.0 {
            PasswordStrenth::Weak
        } else if self >= 36.0 && self < 60.0 {
            PasswordStrenth::Reasonable
        } else if self < 128.0 {
            PasswordStrenth::Strong
        } else {
            PasswordStrenth::VeryStrong
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
        let entropy: PasswordStrenth = password_entropy(&classifier, b"1").into();
        assert_eq!(entropy, PasswordStrenth::VeryWeak);
    }

    #[test]
    fn test_entropy_123456() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrenth = password_entropy(&classifier, b"123456").into();
        assert_eq!(entropy, PasswordStrenth::VeryWeak);
    }

    #[test]
    fn test_entropy_alphabet() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrenth = password_entropy(&classifier, b"abcdef").into();
        assert_eq!(entropy, PasswordStrenth::VeryWeak);
    }

    #[test]
    fn test_entropy_qwerty() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrenth = password_entropy(&classifier, b"QWERTY").into();
        assert_eq!(entropy, PasswordStrenth::Weak);
    }

    #[test]
    fn test_entropy_random_weak() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrenth = password_entropy(&classifier, b"vsPi0v").into();
        assert_eq!(entropy, PasswordStrenth::Weak);
    }

    #[test]
    fn test_entropy_random_reasonable() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrenth = password_entropy(&classifier, b"vsPi0vQk").into();
        assert_eq!(entropy, PasswordStrenth::Reasonable);
    }

    #[test]
    fn test_entropy_random_strong() {
        let classifier = AsciiClassifier;
        let entropy: PasswordStrenth = password_entropy(&classifier, b"vsPi0vQk8J0KSHlG").into();
        assert_eq!(entropy, PasswordStrenth::Strong);
    }
}
