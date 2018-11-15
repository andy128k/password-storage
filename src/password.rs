use rand::{thread_rng, seq::SliceRandom};

const GENERATE_PASSWORD_ALPHABET: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
];

const GENERATE_PASSWORD_LENGTH: usize = 16;

pub fn generate_password() -> String {
    let mut rnd = thread_rng();
    GENERATE_PASSWORD_ALPHABET.choose_multiple(&mut rnd, GENERATE_PASSWORD_LENGTH).cloned().collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_length() {
        assert_eq!(generate_password().len(), GENERATE_PASSWORD_LENGTH);
    }
}
