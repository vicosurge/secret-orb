# MANSION
The Mansion tool is used for the following purpose:

1) Create Rooms, Objects (and Decorations), NPCs and Triggers
2) Modify Rooms, Objects (and Decorations), NPCs and Triggers
3) Delete Rooms, Objects (and Decorations), NPCs and Triggers

## Running the tool
For DOS compatibility the name of the file is HOUSE.EXE
but in Linux it is kept as MANSION, both are exactly the
same.

## Binary files

The `structure.dat` file created by Mansion is your game 
file, when loading the game engine by default it will always
assume the structure file is what should be loaded

If there is no structure file you can specify the file
to load by opening the `draco` executable and pointing
at the local file, which must be on the same folder as
the executable.
