use std::collections::HashMap;

fn main() {
    let mut person = HashMap::new();
    person.insert("name", "John");
    person.insert("age", "30");
    person.insert("city", "New York");

    println!("Person: {:?}", person);
    println!("Name: {:?}", person.get("name"));
    println!("Age: {:?}", person.get("age"));

    person.insert("email", "john@example.com");
    println!("After adding email: {:?}", person);

    if let Some(name) = person.get("name") {
        println!("Name exists: {}", name);
    }

    println!("\nIterating over map:");
    for (key, value) in &person {
        println!("  {}: {}", key, value);
    }

    person.remove("email");
    println!("After removing email: {:?}", person);
}
