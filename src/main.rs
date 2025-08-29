use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};

// Game constants - targeting retro specs
const SCREEN_WIDTH: u32 = 320;
const SCREEN_HEIGHT: u32 = 200;
const MAX_INVENTORY: usize = 8;

// EGA 16-color palette (classic!)
#[derive(Clone, Copy, Debug)]
pub struct Color {
    pub r: u8,
    pub g: u8, 
    pub b: u8,
}

pub const EGA_PALETTE: [Color; 16] = [
    Color { r: 0, g: 0, b: 0 },         // Black
    Color { r: 0, g: 0, b: 170 },       // Blue
    Color { r: 0, g: 170, b: 0 },       // Green
    Color { r: 0, g: 170, b: 170 },     // Cyan
    Color { r: 170, g: 0, b: 0 },       // Red
    Color { r: 170, g: 0, b: 170 },     // Magenta
    Color { r: 170, g: 85, b: 0 },      // Brown
    Color { r: 170, g: 170, b: 170 },   // Light Gray
    Color { r: 85, g: 85, b: 85 },      // Dark Gray
    Color { r: 85, g: 85, b: 255 },     // Light Blue
    Color { r: 85, g: 255, b: 85 },     // Light Green
    Color { r: 85, g: 255, b: 255 },    // Light Cyan
    Color { r: 255, g: 85, b: 85 },     // Light Red
    Color { r: 255, g: 85, b: 255 },    // Light Magenta
    Color { r: 255, g: 255, b: 85 },    // Yellow
    Color { r: 255, g: 255, b: 255 },   // White
];

#[derive(Debug, Clone)]
pub struct Item {
    pub id: String,
    pub name: String,
    pub description: String,
    pub usable: bool,
}

#[derive(Debug, Clone)]
pub struct Room {
    pub id: String,
    pub name: String,
    pub description: String,
    pub long_description: String,
    pub image_path: Option<String>,
    pub exits: HashMap<String, String>, // direction -> room_id
    pub items: Vec<String>, // item IDs present in room
    pub visited: bool,
    pub special_actions: Vec<String>, // Custom actions for this room
}

#[derive(Debug)]
pub struct GameState {
    pub current_room: String,
    pub inventory: Vec<String>, // item IDs
    pub rooms: HashMap<String, Room>,
    pub items: HashMap<String, Item>,
    pub game_over: bool,
    pub win: bool,
    pub turn_count: u32,
    pub health: i32,
    pub sanity: i32, // Cosmic horror element!
}

impl GameState {
    pub fn new() -> Self {
        let mut state = GameState {
            current_room: "apartment".to_string(),
            inventory: Vec::new(),
            rooms: HashMap::new(),
            items: HashMap::new(),
            game_over: false,
            win: false,
            turn_count: 0,
            health: 100,
            sanity: 100,
        };
        
        state.initialize_world();
        state
    }
    
    fn initialize_world(&mut self) {
        // Initialize items
        self.items.insert("flashlight".to_string(), Item {
            id: "flashlight".to_string(),
            name: "Flashlight".to_string(),
            description: "A heavy metal flashlight. The battery seems full.".to_string(),
            usable: true,
        });
        
        self.items.insert("apartment_key".to_string(), Item {
            id: "apartment_key".to_string(),
            name: "Apartment Key".to_string(),
            description: "Your apartment key. Nothing special about it... or is there?".to_string(),
            usable: true,
        });
        
        // Initialize starting inventory
        self.inventory.push("apartment_key".to_string());
        
        // Initialize rooms
        let mut apartment = Room {
            id: "apartment".to_string(),
            name: "Your Apartment".to_string(),
            description: "Your cramped studio apartment. The ceiling creaks ominously above.".to_string(),
            long_description: "The familiar chaos of your one-room apartment surrounds you. Books scattered on the floor, a unmade bed in the corner, and that persistent dripping from the kitchenette. But tonight something feels... different. The shadows seem deeper, and that sound from upstairs hasn't stopped for hours.".to_string(),
            image_path: Some("apartment.png".to_string()),
            exits: HashMap::new(),
            items: vec!["flashlight".to_string()],
            visited: false,
            special_actions: vec!["listen".to_string(), "call_landlord".to_string()],
        };
        apartment.exits.insert("north".to_string(), "hallway".to_string());
        
        let mut hallway = Room {
            id: "hallway".to_string(),
            name: "Building Hallway".to_string(),
            description: "A dimly lit hallway with flickering fluorescent lights.".to_string(),
            long_description: "The hallway stretches before you, lit by buzzing fluorescent lights that flicker irregularly. The carpet is a sickly green color, stained with decades of neglect. Apartment doors line both walls, most bearing numbers that seem to shift when you're not looking directly at them. The sound from above is louder here.".to_string(),
            image_path: Some("hallway.png".to_string()),
            exits: HashMap::new(),
            items: Vec::new(),
            visited: false,
            special_actions: vec!["examine_doors".to_string(), "listen_upstairs".to_string()],
        };
        hallway.exits.insert("south".to_string(), "apartment".to_string());
        hallway.exits.insert("up".to_string(), "stairway".to_string());
        hallway.exits.insert("east".to_string(), "basement_door".to_string());
        
        let mut stairway = Room {
            id: "stairway".to_string(),
            name: "Stairway".to_string(),
            description: "A narrow staircase leading to the upper floors. It's much darker here.".to_string(),
            long_description: "The wooden stairs creak under your weight, each step echoing in the confined space. The railing feels oddly warm to the touch despite the cold air. Above, you can see a faint, pulsing light seeping under what must be the door to 2B. The sound is definitely coming from there - a rhythmic thumping, like something heavy being dragged across the floor.".to_string(),
            image_path: Some("stairway.png".to_string()),
            exits: HashMap::new(),
            items: Vec::new(),
            visited: false,
            special_actions: vec!["examine_light".to_string()],
        };
        stairway.exits.insert("down".to_string(), "hallway".to_string());
        stairway.exits.insert("up".to_string(), "second_floor".to_string());
        
        // Add rooms to game state
        self.rooms.insert("apartment".to_string(), apartment);
        self.rooms.insert("hallway".to_string(), hallway);
        self.rooms.insert("stairway".to_string(), stairway);
    }
    
    pub fn get_current_room(&self) -> Option<&Room> {
        self.rooms.get(&self.current_room)
    }
    
    pub fn move_to_room(&mut self, direction: &str) -> bool {
        if let Some(room) = self.rooms.get(&self.current_room) {
            if let Some(new_room_id) = room.exits.get(direction) {
                self.current_room = new_room_id.clone();
                self.turn_count += 1;
                
                // Mark room as visited
                if let Some(room) = self.rooms.get_mut(&self.current_room) {
                    room.visited = true;
                }
                
                return true;
            }
        }
        false
    }
    
    pub fn take_item(&mut self, item_name: &str) -> Result<String, String> {
        if self.inventory.len() >= MAX_INVENTORY {
            return Err("Your inventory is full!".to_string());
        }
        
        let current_room_id = self.current_room.clone();
        if let Some(room) = self.rooms.get_mut(&current_room_id) {
            // Find item in room by name
            let item_pos = room.items.iter().position(|item_id| {
                if let Some(item) = self.items.get(item_id) {
                    item.name.to_lowercase().contains(&item_name.to_lowercase())
                } else {
                    false
                }
            });
            
            if let Some(pos) = item_pos {
                let item_id = room.items.remove(pos);
                self.inventory.push(item_id.clone());
                
                if let Some(item) = self.items.get(&item_id) {
                    return Ok(format!("Taken: {}", item.name));
                }
            }
        }
        
        Err(format!("You can't find '{}' here.", item_name))
    }
    
    pub fn use_item(&mut self, item_name: &str) -> String {
        // Find item in inventory
        let item_id = self.inventory.iter().find(|&id| {
            if let Some(item) = self.items.get(id) {
                item.name.to_lowercase().contains(&item_name.to_lowercase())
            } else {
                false
            }
        });
        
        if let Some(id) = item_id {
            let id = id.clone();
            match id.as_str() {
                "flashlight" => {
                    "You turn on the flashlight. The beam cuts through the darkness, revealing details you wish you hadn't seen...".to_string()
                }
                _ => "You can't use that right now.".to_string()
            }
        } else {
            "You don't have that item.".to_string()
        }
    }
    
    pub fn look_around(&self) -> String {
        if let Some(room) = self.get_current_room() {
            let mut result = if room.visited {
                format!("{}\n{}", room.name, room.description)
            } else {
                format!("{}\n{}", room.name, room.long_description)
            };
            
            // List items in room
            if !room.items.is_empty() {
                result.push_str("\n\nYou can see:");
                for item_id in &room.items {
                    if let Some(item) = self.items.get(item_id) {
                        result.push_str(&format!("\n  - {}", item.name));
                    }
                }
            }
            
            // List exits
            if !room.exits.is_empty() {
                result.push_str("\n\nExits:");
                for direction in room.exits.keys() {
                    result.push_str(&format!(" {}", direction));
                }
            }
            
            result
        } else {
            "You are nowhere. This should not happen.".to_string()
        }
    }
    
    pub fn show_inventory(&self) -> String {
        if self.inventory.is_empty() {
            "Your inventory is empty.".to_string()
        } else {
            let mut result = "Inventory:".to_string();
            for item_id in &self.inventory {
                if let Some(item) = self.items.get(item_id) {
                    result.push_str(&format!("\n  - {}", item.name));
                }
            }
            result
        }
    }
    
    pub fn show_status(&self) -> String {
        format!(
            "Health: {}/100 | Sanity: {}/100 | Turns: {}",
            self.health, self.sanity, self.turn_count
        )
    }
}

// Simple text-based interface for now (we'll add SDL2 graphics later)
pub struct GameEngine {
    state: GameState,
}

impl GameEngine {
    pub fn new() -> Self {
        GameEngine {
            state: GameState::new(),
        }
    }
    
    pub fn run(&mut self) {
        println!("🌙 THE SOUND FROM ABOVE 🌙");
        println!("A cosmic horror adventure");
        println!("═══════════════════════════════");
        println!();
        
        // Show initial room
        println!("{}", self.state.look_around());
        println!();
        
        while !self.state.game_over && !self.state.win {
            print!("> ");
            io::stdout().flush().unwrap();
            
            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_ok() {
                let command = input.trim().to_lowercase();
                let result = self.process_command(&command);
                println!("{}", result);
                println!();
                
                // Show status every few turns
                if self.state.turn_count % 5 == 0 {
                    println!("[{}]", self.state.show_status());
                    println!();
                }
            }
        }
        
        if self.state.game_over {
            println!("💀 GAME OVER 💀");
        } else if self.state.win {
            println!("🎉 YOU ESCAPED THE HORROR! 🎉");
        }
    }
    
    fn process_command(&mut self, command: &str) -> String {
        let words: Vec<&str> = command.split_whitespace().collect();
        if words.is_empty() {
            return "What?".to_string();
        }
        
        match words[0] {
            "look" | "l" => self.state.look_around(),
            "inventory" | "inv" | "i" => self.state.show_inventory(),
            "take" | "get" => {
                if words.len() > 1 {
                    match self.state.take_item(words[1]) {
                        Ok(msg) => msg,
                        Err(msg) => msg,
                    }
                } else {
                    "Take what?".to_string()
                }
            }
            "use" => {
                if words.len() > 1 {
                    self.state.use_item(words[1])
                } else {
                    "Use what?".to_string()
                }
            }
            "north" | "n" => {
                if self.state.move_to_room("north") {
                    self.state.look_around()
                } else {
                    "You can't go that way.".to_string()
                }
            }
            "south" | "s" => {
                if self.state.move_to_room("south") {
                    self.state.look_around()
                } else {
                    "You can't go that way.".to_string()
                }
            }
            "east" | "e" => {
                if self.state.move_to_room("east") {
                    self.state.look_around()
                } else {
                    "You can't go that way.".to_string()
                }
            }
            "west" | "w" => {
                if self.state.move_to_room("west") {
                    self.state.look_around()
                } else {
                    "You can't go that way.".to_string()
                }
            }
            "up" | "u" => {
                if self.state.move_to_room("up") {
                    self.state.look_around()
                } else {
                    "You can't go that way.".to_string()
                }
            }
            "down" | "d" => {
                if self.state.move_to_room("down") {
                    self.state.look_around()
                } else {
                    "You can't go that way.".to_string()
                }
            }
            "status" => self.state.show_status(),
            "help" => {
                "Commands:\n\
                 look, l - Look around\n\
                 inventory, inv, i - Check inventory\n\
                 take <item> - Take an item\n\
                 use <item> - Use an item\n\
                 north/south/east/west/up/down (or n/s/e/w/u/d) - Move\n\
                 status - Show health/sanity\n\
                 quit - Exit game".to_string()
            }
            "quit" | "exit" => {
                self.state.game_over = true;
                "Thanks for playing!".to_string()
            }
            _ => "I don't understand that command. Type 'help' for available commands.".to_string(),
        }
    }
}

fn main() {
    let mut game = GameEngine::new();
    game.run();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_game_state_creation() {
        let state = GameState::new();
        assert_eq!(state.current_room, "apartment");
        assert_eq!(state.health, 100);
        assert_eq!(state.sanity, 100);
        assert!(!state.game_over);
        assert!(!state.win);
    }

    #[test]
    fn test_room_movement() {
        let mut state = GameState::new();
        assert!(state.move_to_room("north"));
        assert_eq!(state.current_room, "hallway");
        assert_eq!(state.turn_count, 1);
    }

    #[test]
    fn test_invalid_movement() {
        let mut state = GameState::new();
        assert!(!state.move_to_room("banana"));
        assert_eq!(state.current_room, "apartment");
        assert_eq!(state.turn_count, 0);
    }

    #[test]
    fn test_take_item() {
        let mut state = GameState::new();
        let result = state.take_item("flashlight");
        assert!(result.is_ok());
        assert!(state.inventory.contains(&"flashlight".to_string()));
    }

    #[test]
    fn test_inventory_limit() {
        let mut state = GameState::new();
        // Fill inventory to max
        for i in 0..MAX_INVENTORY {
            state.inventory.push(format!("item_{}", i));
        }
        let result = state.take_item("flashlight");
        assert!(result.is_err());
    }
}
