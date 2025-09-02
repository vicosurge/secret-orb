use color_eyre::eyre::{Ok, Result};

fn main() -> Result<()> {
   println!("Hello world!");
   color_eyre::install()?;
   
   let terminal = ratatui::init();
   let result = run(terminal);

   ratatui::restore();
   result
}

fn run(mut terminal: ratatui::DefaultTerminal) -> Result<()> {
   loop {
      //Rendering



      //Input handling
      if let crossterm::event::Event::Key(key) = crossterm::event::read()? {
         match key.code {
            crossterm::event::KeyCode::Esc => {
               break;
            }
            _ => {}
         }
      }

   }
   Ok(())

}
