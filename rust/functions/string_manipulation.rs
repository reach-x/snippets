fn main() {
    let text = "  Hello, World!  ";

    println!("Original: '{}'", text);
    println!("Upper: '{}'", text.to_uppercase());
    println!("Lower: '{}'", text.to_lowercase());
    println!("Trim: '{}'", text.trim());
    println!("Replace: '{}'", text.replace("World", "Rust"));
    println!("Split: {:?}", text.trim().split(", ").collect::<Vec<&str>>());
    println!("Contains: {}", text.contains("World"));
    println!("Starts with: {}", text.trim().starts_with("Hello"));
    println!("Ends with: {}", text.trim().ends_with("!"));
    println!("Length: {}", text.trim().len());
    println!("Repeat: '{}'", "-".repeat(20));
}
