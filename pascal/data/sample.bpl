# Sample Secret Orb World (BPL Format)
# This file demonstrates the BPL syntax for Secret Orb

{START:WORLD}
{REVISION:1}{TITLE:The Lost Caverns}{START:R1}
{END}

# Room definitions

{START:ROOM}
{REVISION:1}
{OC:1}{VAR:R1}{NAME:Cave Entrance}+++
{DESC:You stand at the mouth of a dark cave. Sunlight streams in from behind you, illuminating moss-covered walls. A cool breeze carries the scent of earth and ancient stone.}+++
{NORTH:2}{SOUTH:0}{EAST:0}{WEST:0}{UP:0}{DOWN:0}
{END}

{START:ROOM}
{REVISION:1}
{OC:2}{VAR:R2}{NAME:Narrow Passage}+++
{DESC:The passage narrows here, forcing you to duck beneath low-hanging stalactites. Water drips steadily from above, forming small pools on the uneven floor.}+++
{NORTH:3}{SOUTH:1}{EAST:0}{WEST:0}{UP:0}{DOWN:0}
{END}

{START:ROOM}
{REVISION:1}
{OC:3}{VAR:R3}{NAME:Underground Chamber}+++
{DESC:A vast chamber opens before you. Crystalline formations glitter on the walls, catching the faint phosphorescent glow of strange fungi. An altar stands in the center.}+++
{NORTH:0}{SOUTH:2}{EAST:4}{WEST:0}{UP:0}{DOWN:5}
{END}

{START:ROOM}
{REVISION:1}
{OC:4}{VAR:R4}{NAME:Crystal Alcove}+++
{DESC:This small alcove is filled with crystal formations. They hum with a strange energy, casting rainbow patterns across the walls.}+++
{NORTH:0}{SOUTH:0}{EAST:0}{WEST:3}{UP:0}{DOWN:0}
{END}

{START:ROOM}
{REVISION:1}
{OC:5}{VAR:R5}{NAME:Deep Pit}+++
{DESC:You have descended into a deep pit beneath the chamber. Ancient bones litter the floor, and strange markings cover the walls. In the center rests a glowing orb.}+++
{NORTH:0}{SOUTH:0}{EAST:0}{WEST:0}{UP:3}{DOWN:0}
{END}

# Object definitions

{START:OBJECT}
{REVISION:1}
{OC:1}{VAR:O1}{NAME:Torch}+++
{DESC:A sturdy wooden torch wrapped in oil-soaked cloth.}+++
{ROOM:1}{FLAGS:pickup,use}+++
{USETEXT:The torch flares to life, casting flickering shadows on the cave walls.}
{END}

{START:OBJECT}
{REVISION:1}
{OC:2}{VAR:O2}{NAME:Ancient Scroll}+++
{DESC:A yellowed scroll covered in faded writing.}+++
{ROOM:4}{FLAGS:pickup,read}+++
{USETEXT:The scroll crumbles as you try to read it, but you glimpse the words: "The orb grants wisdom to those who seek truth."}
{END}

{START:OBJECT}
{REVISION:1}
{OC:3}{VAR:O3}{NAME:Glowing Orb}+++
{DESC:A perfectly spherical orb that pulses with inner light. It feels warm to the touch.}+++
{ROOM:5}{FLAGS:pickup,use}+++
{USETEXT:The orb's light intensifies, and for a moment you glimpse visions of a world beyond...}
{END}

{START:OBJECT}
{REVISION:1}
{OC:4}{VAR:O4}{NAME:Rusty Key}+++
{DESC:An old iron key covered in rust. It might still work.}+++
{ROOM:3}{FLAGS:pickup,use}+++
{USETEXT:You try the key, but there's nothing here to use it on.}
{END}

# Mob definitions

{START:MOB}
{REVISION:1}
{OC:1}{VAR:M1}{NAME:Cave Hermit}+++
{DESC:An old man in tattered robes sits near the entrance, watching you with keen eyes.}+++
{ROOM:1}{DIALOGUE:Seek the orb in the depths below. But beware - not all treasures are meant to be taken.}
{END}

{START:MOB}
{REVISION:1}
{OC:2}{VAR:M2}{NAME:Stone Guardian}+++
{DESC:A statue that seems almost alive. Its eyes follow your movements.}+++
{ROOM:3}{DIALOGUE:Only the worthy may pass. Prove your worth or turn back.}
{END}
