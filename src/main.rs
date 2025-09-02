use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write}; // Added for input/output

#[derive(Deserialize, Debug)]
struct Room {
    name: String,
    id: u32,
    description: String,
}

#[derive(Deserialize, Debug)]
struct RoomData {
    rooms: HashMap<String, Room>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let json_string = fs::read_to_string("rooms.json")?;
    let data: RoomData = serde_json::from_str(&json_string)?;
    
    loop {
        // Prompt for input
        print!("Enter room ID to search (or 'quit' to exit): ");
        io::stdout().flush()?; // Ensure prompt appears before input
        
        // Read user input
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let input = input.trim(); // Remove newline/whitespace
        
        // Check if user wants to quit
        if input.to_lowercase() == "quit" {
            println!("Goodbye!");
            break;
        }
        
        // Search for the room
        match data.rooms.get(input) {
            Some(room) => {
                println!("Found room:");
                println!("  Name: {}", room.name);
                println!("  ID: {}", room.id);
                println!("  Description: {}", room.description);
            }
            None => {
                println!("Room with ID '{}' not found.", input);
                // Optionally show available room IDs
                println!("Available room IDs: {:?}", 
                    data.rooms.keys().collect::<Vec<_>>());
            }
        }
        println!(); // Empty line for readability
    }
    
    Ok(())
}
