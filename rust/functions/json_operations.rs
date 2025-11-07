use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug)]
struct Person {
    name: String,
    age: u32,
    skills: Vec<String>,
    active: bool,
}

fn main() {
    let person = Person {
        name: String::from("John Doe"),
        age: 30,
        skills: vec![
            String::from("Rust"),
            String::from("Go"),
            String::from("Python"),
        ],
        active: true,
    };

    let json_string = serde_json::to_string_pretty(&person).unwrap();
    println!("JSON String:\n{}", json_string);

    let parsed_person: Person = serde_json::from_str(&json_string).unwrap();
    println!("\nParsed data: {:?}", parsed_person);
    println!("Name: {}", parsed_person.name);
    println!("Skills: {:?}", parsed_person.skills);

    // Note: To use this example, add to Cargo.toml:
    // [dependencies]
    // serde = { version = "1.0", features = ["derive"] }
    // serde_json = "1.0"
}
