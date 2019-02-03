use std::convert::Into;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CharClass {
    Control,
    Number,
    Upper,
    Lower,
    Punct1,
    Punct2,
    Extended
}

fn create_classes() -> [CharClass; 256] {
    let mut classes = [CharClass::Extended; 256];
    for c in 0..32 {
        classes[c] = CharClass::Control;
    }
    for c in 48..58 {
        classes[c] = CharClass::Number;
    }
    for c in 65..91 {
        classes[c] = CharClass::Upper;
    }
    for c in 97..123 {
        classes[c] = CharClass::Lower;
    }
    for c in &[32, 33, 35, 36, 37, 38, 40, 41, 42, 43, 45, 47, 61, 64, 94, 95] {
        classes[*c] = CharClass::Punct1;
    }
    for c in &[34, 39, 44, 46, 58, 59, 60, 62, 63, 91, 92, 93, 96, 123, 127] {
        classes[*c] = CharClass::Punct2;
    }
    for c in 128..256 {
        classes[c] = CharClass::Extended;
    }
    classes
}

fn class_sizes(map: &[CharClass]) -> HashMap<CharClass, u8> {
    let mut result = HashMap::new();
    for class in map.iter() {
        let counter = result.entry(*class).or_insert(0);
        *counter += 1;
    }
    result
}

pub struct AsciiClasses {
    classes: [CharClass; 256],
    sizes: HashMap<CharClass, u8>,
}

impl AsciiClasses {
    pub fn new() -> Self {
        let classes = create_classes();
        let sizes = class_sizes(&classes);
        Self { classes, sizes }
    }
}

pub trait CharClassifier<T> {
    fn classify(&self, ch: u8) -> T;
    fn class_size(&self, class: T) -> u32;
}

impl CharClassifier<CharClass> for AsciiClasses {
    fn classify(&self, ch: u8) -> CharClass {
        self.classes[ch as usize]
    }

    fn class_size(&self, class: CharClass) -> u32 {
        *self.sizes.get(&class).unwrap_or(&0u8) as u32
    }
}

fn character_distance(ch1: u8, ch2: u8) -> i32 {
    // TODO: check in known sequences... qwertyuiop, PI digits...
    ((ch1 as i32) - (ch2 as i32)).abs()
}

pub fn password_entropy<T>(classifier: &dyn CharClassifier<T>, passw: &[u8]) -> f32
    where T: Eq + std::hash::Hash
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
    VeryStrong
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
    fn test_entropy_1() {
        let classifier = AsciiClasses::new();
        let entropy: PasswordStrenth = password_entropy(&classifier, b"1").into();
        assert_eq!(entropy, PasswordStrenth::VeryWeak);
    }

    #[test]
    fn test_entropy_123456() {
        let classifier = AsciiClasses::new();
        let entropy: PasswordStrenth = password_entropy(&classifier, b"123456").into();
        assert_eq!(entropy, PasswordStrenth::VeryWeak);
    }

    #[test]
    fn test_entropy_alphabet() {
        let classifier = AsciiClasses::new();
        let entropy: PasswordStrenth = password_entropy(&classifier, b"abcdef").into();
        assert_eq!(entropy, PasswordStrenth::VeryWeak);
    }

    #[test]
    fn test_entropy_qwerty() {
        let classifier = AsciiClasses::new();
        let entropy: PasswordStrenth = password_entropy(&classifier, b"QWERTY").into();
        assert_eq!(entropy, PasswordStrenth::Weak);
    }

    #[test]
    fn test_entropy_random_weak() {
        let classifier = AsciiClasses::new();
        let entropy: PasswordStrenth = password_entropy(&classifier, b"vsPi0v").into();
        assert_eq!(entropy, PasswordStrenth::Weak);
    }

    #[test]
    fn test_entropy_random_reasonable() {
        let classifier = AsciiClasses::new();
        let entropy: PasswordStrenth = password_entropy(&classifier, b"vsPi0vQk").into();
        assert_eq!(entropy, PasswordStrenth::Reasonable);
    }

    #[test]
    fn test_entropy_random_strong() {
        let classifier = AsciiClasses::new();
        let entropy: PasswordStrenth = password_entropy(&classifier, b"vsPi0vQk8J0KSHlG").into();
        assert_eq!(entropy, PasswordStrenth::Strong);
    }
}
