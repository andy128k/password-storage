pub fn adjust_password(password: &str) -> [u8; 32] {
    let bytes = password.as_bytes();
    let len = usize::min(32, bytes.len());
    let mut array = [0; 32];
    array[..len].copy_from_slice(&bytes[..len]);
    array
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_adjust_password() {
        assert_eq!(
            adjust_password("secr3t"),
            [
                0x73, 0x65, 0x63, 0x72, 0x33, 0x74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            ]
        );
        assert_eq!(
            adjust_password("secr3tsecr3tsecr3tsecr3tsecr3tsecr3tsecr3t"),
            [
                0x73, 0x65, 0x63, 0x72, 0x33, 0x74, 0x73, 0x65, 0x63, 0x72, 0x33, 0x74, 0x73, 0x65,
                0x63, 0x72, 0x33, 0x74, 0x73, 0x65, 0x63, 0x72, 0x33, 0x74, 0x73, 0x65, 0x63, 0x72,
                0x33, 0x74, 0x73, 0x65
            ]
        );
    }
}
