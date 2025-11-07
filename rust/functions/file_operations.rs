use std::fs;
use std::io::Write;

fn main() {
    let test_file = "../tmp/test_output.txt";

    let data = "Hello from Rust!\nThis is a test file.\n";
    fs::write(test_file, data).expect("Unable to write file");
    println!("Written to {}", test_file);

    let content = fs::read_to_string(test_file).expect("Unable to read file");
    println!("Read content:\n{}", content);

    if let Ok(metadata) = fs::metadata(test_file) {
        println!("File exists: {}", test_file);
        println!("File size: {} bytes", metadata.len());
    }

    let mut file = fs::OpenOptions::new()
        .append(true)
        .open(test_file)
        .expect("Unable to open file");

    file.write_all(b"Appended line\n").expect("Unable to append");
    println!("Appended to file");
}
