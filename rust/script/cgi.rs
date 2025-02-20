use serde::{Serialize, Deserialize};
use serde_json::json;
use std::collections::HashMap;
use std::env;
use query_string::querify;

#[derive(Serialize, Deserialize)]
struct Response {
    environment: HashMap<String, String>,
    parameters: HashMap<String, String>,
}

fn main() {
    // Get the query string from the environment variable
    let query_string = env::var("QUERY_STRING").unwrap_or_else(|_| "".into());

    // Parse the query string into a HashMap
    let parsed_params: HashMap<String, String> = querify(&query_string)
        .into_iter()
        .map(|(key, value)| (key.to_string(), sanitize_input(value)))
        .collect();

    // Collect environment variables
    let environment: HashMap<String, String> = env::vars().collect();

    // Create the response object
    let response = Response {
        environment,
        parameters: parsed_params,
    };

    // Serialize it to JSON
    let serialized = serde_json::to_string(&response).unwrap();

    // Print the JSON as the response
    println!("Content-Type: application/json\n");
    println!("{}", serialized);
}

fn sanitize_input(input: &str) -> String {
    // Implement input sanitization here. This example replaces "<" and ">" as a basic measure.
    // Adapt this function based on your needs and security considerations.
    input.replace("<", "&lt;").replace(">", "&gt;")
}

