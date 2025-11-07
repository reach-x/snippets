pub fn clean_phone(phone_number: &str) -> String {
    phone_number.chars().filter(|c| c.is_numeric()).collect()
}

pub fn to_snake_case(text: &str) -> String {
    let mut result = String::new();
    for (i, ch) in text.chars().enumerate() {
        if ch.is_uppercase() && i > 0 {
            result.push('_');
        }
        result.push(ch.to_lowercase().next().unwrap());
    }
    result
}

pub fn to_camel_case(text: &str) -> String {
    let parts: Vec<&str> = text.split('_').collect();
    let mut result = String::from(parts[0]);

    for part in &parts[1..] {
        if !part.is_empty() {
            let mut chars = part.chars();
            if let Some(first) = chars.next() {
                result.push(first.to_uppercase().next().unwrap());
                result.push_str(&chars.collect::<String>());
            }
        }
    }
    result
}

pub fn truncate(text: &str, length: usize, suffix: &str) -> String {
    if text.len() <= length {
        return text.to_string();
    }
    format!("{}{}", &text[..length - suffix.len()], suffix)
}

pub fn slugify(text: &str) -> String {
    text.to_lowercase()
        .trim()
        .chars()
        .map(|c| if c.is_alphanumeric() || c == ' ' || c == '-' { c } else { ' ' })
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<&str>>()
        .join("-")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clean_phone() {
        assert_eq!(clean_phone("1-800-555-1234"), "18005551234");
    }

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("userName"), "user_name");
    }

    #[test]
    fn test_to_camel_case() {
        assert_eq!(to_camel_case("user_name"), "userName");
    }
}
