// Note: Add to Cargo.toml:
// [dependencies]
// reqwest = { version = "0.11", features = ["blocking", "json"] }
// serde_json = "1.0"

use reqwest::blocking::Client;
use serde_json::Value;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = Client::new();
    let url = "https://api.github.com/users/github";

    let response = client
        .get(url)
        .header("User-Agent", "Rust HTTP Client")
        .send()?;

    println!("Status Code: {}", response.status());

    let json: Value = response.json()?;

    println!("\nUser: {}", json["name"]);
    println!("Bio: {}", json["bio"]);
    println!("Public repos: {}", json["public_repos"]);

    Ok(())
}
