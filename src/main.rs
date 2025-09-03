use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Clear, List, ListItem, ListState, Paragraph, Wrap},
    Frame, Terminal,
};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    error::Error,
    fs,
    io,
};

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Room {
    name: String,
    id: u32,
    description: String,
    north: u32,
    south: u32,
    east: u32,
    west: u32,
}

#[derive(Serialize, Deserialize, Debug)]
struct RoomData {
    rooms: HashMap<String, Room>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum AppState {
    MainMenu,
    RoomList,
    AddRoom,
    EditRoom,
    DeleteConfirm,
    SearchRoom,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FormField {
    Name,
    Description,
    North,
    South,
    East,
    West,
}

struct App {
    state: AppState,
    room_data: RoomData,
    main_menu_state: ListState,
    room_list_state: ListState,
    form_field_state: FormField,
    form_name: String,
    form_description: String,
    form_north: String,
    form_south: String,
    form_east: String,
    form_west: String,
    selected_room_id: Option<u32>,
    search_input: String,
    error_message: Option<String>,
    should_quit: bool,
    filename: String,
}

impl App {
    fn new(filename: String) -> Result<App, Box<dyn Error>> {
        let room_data = if std::path::Path::new(&filename).exists() {
            let json_string = fs::read_to_string(&filename)?;
            serde_json::from_str(&json_string)?
        } else {
            RoomData {
                rooms: HashMap::new(),
            }
        };

        let mut app = App {
            state: AppState::MainMenu,
            room_data,
            main_menu_state: ListState::default(),
            room_list_state: ListState::default(),
            form_field_state: FormField::Name,
            form_name: String::new(),
            form_description: String::new(),
            form_north: String::from("0"),
            form_south: String::from("0"),
            form_east: String::from("0"),
            form_west: String::from("0"),
            selected_room_id: None,
            search_input: String::new(),
            error_message: None,
            should_quit: false,
            filename,
        };

        app.main_menu_state.select(Some(0));
        Ok(app)
    }

    fn save_to_file(&self) -> Result<(), Box<dyn Error>> {
        let json_string = serde_json::to_string_pretty(&self.room_data)?;
        fs::write(&self.filename, json_string)?;
        Ok(())
    }

    fn get_next_available_id(&self) -> u32 {
        let mut id = 1;
        loop {
            if !self.room_data.rooms.contains_key(&id.to_string()) {
                return id;
            }
            id += 1;
        }
    }

    fn clear_form(&mut self) {
        self.form_name.clear();
        self.form_description.clear();
        self.form_north = String::from("0");
        self.form_south = String::from("0");
        self.form_east = String::from("0");
        self.form_west = String::from("0");
        self.form_field_state = FormField::Name;
    }

    fn populate_form_for_edit(&mut self, room: &Room) {
        self.form_name = room.name.clone();
        self.form_description = room.description.clone();
        self.form_north = room.north.to_string();
        self.form_south = room.south.to_string();
        self.form_east = room.east.to_string();
        self.form_west = room.west.to_string();
        self.form_field_state = FormField::Name;
    }

    fn validate_and_save_room(&mut self, is_edit: bool) -> Result<(), String> {
        if self.form_name.trim().is_empty() {
            return Err("Room name cannot be empty".to_string());
        }

        let north = self.form_north.parse::<u32>().unwrap_or(0);
        let south = self.form_south.parse::<u32>().unwrap_or(0);
        let east = self.form_east.parse::<u32>().unwrap_or(0);
        let west = self.form_west.parse::<u32>().unwrap_or(0);

        let id = if is_edit {
            self.selected_room_id.unwrap()
        } else {
            self.get_next_available_id()
        };

        let room = Room {
            name: self.form_name.trim().to_string(),
            id,
            description: self.form_description.trim().to_string(),
            north,
            south,
            east,
            west,
        };

        self.room_data.rooms.insert(id.to_string(), room);
        
        if let Err(e) = self.save_to_file() {
            return Err(format!("Failed to save: {}", e));
        }

        self.clear_form();
        self.state = AppState::RoomList;
        Ok(())
    }

    fn delete_selected_room(&mut self) -> Result<(), String> {
        if let Some(id) = self.selected_room_id {
            self.room_data.rooms.remove(&id.to_string());
            if let Err(e) = self.save_to_file() {
                return Err(format!("Failed to save: {}", e));
            }
            self.state = AppState::RoomList;
        }
        Ok(())
    }

    fn get_room_list(&self) -> Vec<Room> {
        let mut rooms: Vec<Room> = self.room_data.rooms.values().cloned().collect();
        rooms.sort_by_key(|r| r.id);
        rooms
    }
}

fn handle_key_events(app: &mut App, key: KeyCode) -> Result<(), Box<dyn Error>> {
    match app.state {
        AppState::MainMenu => match key {
            KeyCode::Down => {
                let len = 6; // Number of menu items
                let i = app.main_menu_state.selected().unwrap_or(0);
                app.main_menu_state.select(Some((i + 1) % len));
            }
            KeyCode::Up => {
                let len = 6;
                let i = app.main_menu_state.selected().unwrap_or(0);
                app.main_menu_state.select(Some(if i == 0 { len - 1 } else { i - 1 }));
            }
            KeyCode::Enter => {
                app.error_message = None; // Clear any existing messages
                match app.main_menu_state.selected() {
                    Some(0) => {
                        app.state = AppState::RoomList;
                        if !app.get_room_list().is_empty() {
                            app.room_list_state.select(Some(0));
                        }
                    }
                    Some(1) => {
                        app.state = AppState::AddRoom;
                        app.clear_form();
                    }
                    Some(2) => {
                        app.state = AppState::SearchRoom;
                        app.search_input.clear();
                    }
                    Some(3) => {
                        if !app.get_room_list().is_empty() {
                            app.state = AppState::RoomList;
                            app.room_list_state.select(Some(0));
                        }
                    }
                    Some(4) => {
                        if !app.get_room_list().is_empty() {
                            app.state = AppState::RoomList;
                            app.room_list_state.select(Some(0));
                        }
                    }
                    Some(5) => app.should_quit = true,
                    _ => {}
                }
            }
            KeyCode::Char('q') => app.should_quit = true,
            _ => {}
        }
        AppState::RoomList => match key {
            KeyCode::Down => {
                let rooms = app.get_room_list();
                if !rooms.is_empty() {
                    let i = app.room_list_state.selected().unwrap_or(0);
                    app.room_list_state.select(Some((i + 1) % rooms.len()));
                }
            }
            KeyCode::Up => {
                let rooms = app.get_room_list();
                if !rooms.is_empty() {
                    let i = app.room_list_state.selected().unwrap_or(0);
                    app.room_list_state.select(Some(if i == 0 { rooms.len() - 1 } else { i - 1 }));
                }
            }
            KeyCode::Char('e') => {
                if let Some(selected) = app.room_list_state.selected() {
                    let rooms = app.get_room_list();
                    if let Some(room) = rooms.get(selected) {
                        app.selected_room_id = Some(room.id);
                        app.populate_form_for_edit(room);
                        app.state = AppState::EditRoom;
                    }
                }
            }
            KeyCode::Char('d') => {
                if let Some(selected) = app.room_list_state.selected() {
                    let rooms = app.get_room_list();
                    if let Some(room) = rooms.get(selected) {
                        app.selected_room_id = Some(room.id);
                        app.state = AppState::DeleteConfirm;
                    }
                }
            }
            KeyCode::Esc => {
                app.error_message = None; // Clear messages when going back
                app.state = AppState::MainMenu;
            }
            _ => {}
        }
        AppState::AddRoom | AppState::EditRoom => {
            match key {
                KeyCode::Tab | KeyCode::Down => {
                    app.form_field_state = match app.form_field_state {
                        FormField::Name => FormField::Description,
                        FormField::Description => FormField::North,
                        FormField::North => FormField::South,
                        FormField::South => FormField::East,
                        FormField::East => FormField::West,
                        FormField::West => FormField::Name,
                    };
                }
                KeyCode::BackTab | KeyCode::Up => {
                    app.form_field_state = match app.form_field_state {
                        FormField::Name => FormField::West,
                        FormField::Description => FormField::Name,
                        FormField::North => FormField::Description,
                        FormField::South => FormField::North,
                        FormField::East => FormField::South,
                        FormField::West => FormField::East,
                    };
                }
                KeyCode::Enter => {
                    let is_edit = app.state == AppState::EditRoom;
                    match app.validate_and_save_room(is_edit) {
                        Ok(()) => app.error_message = None,
                        Err(e) => app.error_message = Some(e),
                    }
                }
                KeyCode::Esc => {
                    app.error_message = None; // Clear messages when canceling
                    app.state = AppState::MainMenu;
                    app.clear_form();
                }
                KeyCode::Char(c) => {
                    match app.form_field_state {
                        FormField::Name => app.form_name.push(c),
                        FormField::Description => app.form_description.push(c),
                        FormField::North => app.form_north.push(c),
                        FormField::South => app.form_south.push(c),
                        FormField::East => app.form_east.push(c),
                        FormField::West => app.form_west.push(c),
                    }
                }
                KeyCode::Backspace => {
                    match app.form_field_state {
                        FormField::Name => { app.form_name.pop(); }
                        FormField::Description => { app.form_description.pop(); }
                        FormField::North => { app.form_north.pop(); }
                        FormField::South => { app.form_south.pop(); }
                        FormField::East => { app.form_east.pop(); }
                        FormField::West => { app.form_west.pop(); }
                    }
                }
                _ => {}
            }
        }
        AppState::DeleteConfirm => match key {
            KeyCode::Char('y') | KeyCode::Char('Y') => {
                match app.delete_selected_room() {
                    Ok(()) => app.error_message = None,
                    Err(e) => app.error_message = Some(e),
                }
            }
            KeyCode::Char('n') | KeyCode::Char('N') | KeyCode::Esc => {
                app.state = AppState::RoomList;
            }
            _ => {}
        }
        AppState::SearchRoom => match key {
            KeyCode::Enter => {
                if let Some(room) = app.room_data.rooms.get(&app.search_input) {
                    app.error_message = Some(format!(
                        "Found: {} - {} (N:{} S:{} E:{} W:{})",
                        room.name, room.description, room.north, room.south, room.east, room.west
                    ));
                } else {
                    app.error_message = Some(format!("Room with ID '{}' not found", app.search_input));
                }
            }
            KeyCode::Esc => {
                app.state = AppState::MainMenu;
                app.search_input.clear();
            }
            KeyCode::Char(c) => app.search_input.push(c),
            KeyCode::Backspace => { app.search_input.pop(); }
            _ => {}
        }
    }
    Ok(())
}

fn ui(f: &mut Frame, app: &mut App) {
    match app.state {
        AppState::MainMenu => draw_main_menu(f, app),
        AppState::RoomList => draw_room_list(f, app),
        AppState::AddRoom => draw_room_form(f, app, "Add Room"),
        AppState::EditRoom => draw_room_form(f, app, "Edit Room"),
        AppState::DeleteConfirm => draw_delete_confirm(f, app),
        AppState::SearchRoom => draw_search(f, app),
    }
    
    if let Some(ref msg) = app.error_message {
        draw_popup(f, msg);
    }
}

fn draw_main_menu(f: &mut Frame, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints([Constraint::Min(0)].as_ref())
        .split(f.area());

    let menu_items = vec![
        ListItem::new("List Rooms"),
        ListItem::new("Add Room"),
        ListItem::new("Search Room"),
        ListItem::new("Edit Room"),
        ListItem::new("Delete Room"),
        ListItem::new("Exit"),
    ];

    let menu = List::new(menu_items)
        .block(Block::default().title("Room Manager").borders(Borders::ALL))
        .style(Style::default().fg(Color::White))
        .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
        .highlight_symbol(">> ");

    f.render_stateful_widget(menu, chunks[0], &mut app.main_menu_state);
}

fn draw_room_list(f: &mut Frame, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        .constraints([Constraint::Min(0), Constraint::Length(3)].as_ref())
        .split(f.area());

    let rooms = app.get_room_list();
    let items: Vec<ListItem> = rooms
        .iter()
        .map(|room| {
            ListItem::new(format!(
                "ID: {} | {} - {} | N:{} S:{} E:{} W:{}",
                room.id, room.name, room.description, room.north, room.south, room.east, room.west
            ))
        })
        .collect();

    let list = List::new(items)
        .block(Block::default().title("Rooms").borders(Borders::ALL))
        .style(Style::default().fg(Color::White))
        .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
        .highlight_symbol(">> ");

    f.render_stateful_widget(list, chunks[0], &mut app.room_list_state);

    let help = Paragraph::new("Press 'e' to edit, 'd' to delete, Esc to return to main menu")
        .block(Block::default().borders(Borders::ALL))
        .style(Style::default().fg(Color::Yellow));
    f.render_widget(help, chunks[1]);
}

fn draw_room_form(f: &mut Frame, app: &mut App, title: &str) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints([
            Constraint::Length(3), // Name
            Constraint::Length(3), // Description
            Constraint::Length(3), // North
            Constraint::Length(3), // South
            Constraint::Length(3), // East
            Constraint::Length(3), // West
            Constraint::Min(0),    // Instructions
        ].as_ref())
        .split(f.area());

    let fields = [
        ("Name", &app.form_name, FormField::Name),
        ("Description", &app.form_description, FormField::Description),
        ("North (ID)", &app.form_north, FormField::North),
        ("South (ID)", &app.form_south, FormField::South),
        ("East (ID)", &app.form_east, FormField::East),
        ("West (ID)", &app.form_west, FormField::West),
    ];

    for (i, (label, value, field)) in fields.iter().enumerate() {
        let style = if *field == app.form_field_state {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default().fg(Color::White)
        };

        let input = Paragraph::new(value.as_str())
            .style(style)
            .block(Block::default().borders(Borders::ALL).title(*label));
        f.render_widget(input, chunks[i]);
    }

    let instructions = Paragraph::new("Tab/Arrow keys to navigate fields, Enter to save, Esc to cancel")
        .block(Block::default().title(title).borders(Borders::ALL))
        .style(Style::default().fg(Color::Cyan))
        .wrap(Wrap { trim: true });
    f.render_widget(instructions, chunks[6]);
}

fn draw_delete_confirm(f: &mut Frame, app: &mut App) {
    let popup_area = centered_rect(50, 20, f.area());
    
    if let Some(id) = app.selected_room_id {
        if let Some(room) = app.room_data.rooms.get(&id.to_string()) {
            let text = format!("Delete room '{}'?\n\nPress Y to confirm, N to cancel", room.name);
            let paragraph = Paragraph::new(text)
                .block(Block::default().title("Confirm Delete").borders(Borders::ALL))
                .style(Style::default().fg(Color::Red))
                .alignment(Alignment::Center)
                .wrap(Wrap { trim: true });
            
            f.render_widget(Clear, popup_area);
            f.render_widget(paragraph, popup_area);
        }
    }
}

fn draw_search(f: &mut Frame, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints([Constraint::Length(3), Constraint::Min(0)].as_ref())
        .split(f.area());

    let input = Paragraph::new(app.search_input.as_str())
        .style(Style::default().fg(Color::Yellow))
        .block(Block::default().borders(Borders::ALL).title("Search by Room ID"));
    f.render_widget(input, chunks[0]);

    let help = Paragraph::new("Enter room ID to search, Enter to search, Esc to return")
        .block(Block::default().borders(Borders::ALL))
        .style(Style::default().fg(Color::Cyan))
        .wrap(Wrap { trim: true });
    f.render_widget(help, chunks[1]);
}

fn draw_popup(f: &mut Frame, message: &str) {
    let popup_area = centered_rect(60, 20, f.area());
    
    let paragraph = Paragraph::new(message)
        .block(Block::default().title("Message").borders(Borders::ALL))
        .style(Style::default().fg(Color::White).bg(Color::DarkGray))
        .alignment(Alignment::Center)
        .wrap(Wrap { trim: true });
    
    f.render_widget(Clear, popup_area);
    f.render_widget(paragraph, popup_area);
}

fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
    let popup_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(r);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(popup_layout[1])[1]
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> Result<(), Box<dyn Error>> {
    loop {
        terminal.draw(|f| ui(f, &mut app))?;

        if let Event::Key(key) = event::read()? {
            if key.kind == KeyEventKind::Press {
                match key.code {
                    KeyCode::Char('q') if app.state == AppState::MainMenu => break,
                    KeyCode::Char('c') if key.modifiers.contains(event::KeyModifiers::CONTROL) => break,
                    _ => handle_key_events(&mut app, key.code)?,
                }
            }
        }

        if app.should_quit {
            break;
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();
    let filename = if args.len() > 1 {
        args[1].clone()
    } else {
        "rooms.json".to_string()
    };

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app and run it
    let app = App::new(filename)?;
    let res = run_app(&mut terminal, app);

    // Restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{:?}", err)
    }

    Ok(())
}