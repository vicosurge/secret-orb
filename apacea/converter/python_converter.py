#!/usr/bin/env python3
"""
JSON to Binary Game Data Converter
Converts room data from JSON format to optimized binary format for Pascal reading.
"""

import json
import struct
import sys
import os
from typing import List, Dict, Any

def write_pstring(f, text: str) -> None:
    """Write a Pascal-style string (length byte + data) in Latin-1 encoding."""
    if text is None:
        text = ""
    
    # Encode to Latin-1 and truncate if necessary
    encoded = text.encode('latin-1', errors='replace')
    if len(encoded) > 255:
        encoded = encoded[:255]
        print(f"Warning: String truncated to 255 bytes: '{text[:50]}...'")
    
    # Write length byte + string data
    f.write(struct.pack('B', len(encoded)))
    if encoded:
        f.write(encoded)

def write_string_array(f, strings: List[str]) -> None:
    """Write a dynamic string array (count byte + strings)."""
    if strings is None:
        strings = []
    
    count = len(strings)
    if count > 255:
        count = 255
        strings = strings[:255]
        print(f"Warning: String array truncated to 255 items")
    
    # Write count
    f.write(struct.pack('B', count))
    
    # Write each string
    for s in strings:
        write_pstring(f, s)

def write_header(f, room_count: int) -> None:
    """Write the game data file header."""
    magic = b'GAME'
    version = 1
    room_data_offset = 20  # Size of header
    npc_data_offset = 0    # Not used yet
    object_data_offset = 0 # Not used yet
    
    f.write(magic)
    f.write(struct.pack('<H', version))           # Little-endian 2-byte word
    f.write(struct.pack('<L', room_count))        # Little-endian 4-byte long
    f.write(struct.pack('<L', room_data_offset))  # Little-endian 4-byte long
    f.write(struct.pack('<L', npc_data_offset))   # Little-endian 4-byte long
    f.write(struct.pack('<L', object_data_offset)) # Little-endian 4-byte long

def write_room(f, room: Dict[str, Any]) -> None:
    """Write a single room to the binary file."""
    # Write room ID
    room_id = room.get('id', 0)
    f.write(struct.pack('<L', room_id))
    
    # Write name and description
    write_pstring(f, room.get('name', ''))
    write_pstring(f, room.get('description', ''))
    
    # Write exits (6 x 4 bytes)
    exits = room.get('exits', {})
    directions = ['north', 'south', 'east', 'west', 'up', 'down']
    for direction in directions:
        exit_id = exits.get(direction, -1)
        f.write(struct.pack('<l', exit_id))  # Signed long for -1 values
    
    # Write boolean fields as bytes
    lit = 1 if room.get('lit', False) else 0
    f.write(struct.pack('B', lit))
    
    # Write trigger and NPC IDs
    trigger = room.get('trigger', -1)
    npc = room.get('npc', -1)
    f.write(struct.pack('<l', trigger))
    f.write(struct.pack('<l', npc))
    
    # Write visited flag
    visited = 1 if room.get('visited', False) else 0
    f.write(struct.pack('B', visited))
    
    # Write dynamic arrays
    write_string_array(f, room.get('tags', []))
    write_string_array(f, room.get('hazards', []))
    write_string_array(f, room.get('fixtures', []))
    write_string_array(f, room.get('event_flags', []))

def convert_json_to_binary(json_file: str, binary_file: str = None) -> bool:
    """Convert JSON game data to binary format."""
    
    # Default output filename
    if binary_file is None:
        base_name = os.path.splitext(json_file)[0]
        binary_file = f"{base_name}.bin"
    
    try:
        # Read JSON file
        print(f"Reading JSON file: {json_file}")
        with open(json_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        # Extract rooms
        rooms = data.get('rooms', [])
        room_count = len(rooms)
        
        if room_count == 0:
            print("Warning: No rooms found in JSON file")
            return False
        
        print(f"Found {room_count} rooms")
        
        # Write binary file
        print(f"Writing binary file: {binary_file}")
        with open(binary_file, 'wb') as f:
            # Write header
            write_header(f, room_count)
            
            # Write all rooms
            for i, room in enumerate(rooms):
                write_room(f, room)
                if (i + 1) % 10 == 0:
                    print(f"  Processed {i + 1}/{room_count} rooms...")
        
        # Get file sizes
        json_size = os.path.getsize(json_file)
        binary_size = os.path.getsize(binary_file)
        
        print(f"\nConversion completed successfully!")
        print(f"JSON file size:   {json_size:,} bytes")
        print(f"Binary file size: {binary_size:,} bytes")
        print(f"Size reduction:   {((json_size - binary_size) / json_size * 100):.1f}%")
        
        return True
        
    except FileNotFoundError:
        print(f"Error: File '{json_file}' not found")
        return False
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in '{json_file}': {e}")
        return False
    except Exception as e:
        print(f"Error: {e}")
        return False

def verify_binary_file(binary_file: str) -> bool:
    """Verify the binary file format (basic check)."""
    try:
        with open(binary_file, 'rb') as f:
            # Check magic signature
            magic = f.read(4)
            if magic != b'GAME':
                print(f"Error: Invalid magic signature in '{binary_file}'")
                return False
            
            # Read version and room count
            version = struct.unpack('<H', f.read(2))[0]
            room_count = struct.unpack('<L', f.read(4))[0]
            
            print(f"Binary file verification:")
            print(f"  Magic: {magic.decode('ascii')}")
            print(f"  Version: {version}")
            print(f"  Room count: {room_count}")
            
            return True
            
    except Exception as e:
        print(f"Error verifying binary file: {e}")
        return False

def print_usage():
    """Print usage information."""
    print("Usage: python json_to_binary.py <json_file> [output_file]")
    print("")
    print("Arguments:")
    print("  json_file    Input JSON file containing room data")
    print("  output_file  Output binary file (optional, defaults to input_name.bin)")
    print("")
    print("Options:")
    print("  --verify     Verify the output binary file after conversion")
    print("  --help       Show this help message")
    print("")
    print("Example:")
    print("  python json_to_binary.py structure.json")
    print("  python json_to_binary.py structure.json gamedata.bin --verify")

def main():
    """Main entry point."""
    args = sys.argv[1:]
    
    if not args or '--help' in args or '-h' in args:
        print_usage()
        return
    
    # Parse arguments
    json_file = None
    binary_file = None
    verify = False
    
    for i, arg in enumerate(args):
        if arg == '--verify':
            verify = True
        elif not arg.startswith('--'):
            if json_file is None:
                json_file = arg
            elif binary_file is None:
                binary_file = arg
    
    if json_file is None:
        print("Error: JSON file argument required")
        print_usage()
        return
    
    # Convert file
    success = convert_json_to_binary(json_file, binary_file)
    
    if success and verify:
        output_file = binary_file if binary_file else f"{os.path.splitext(json_file)[0]}.bin"
        print()
        verify_binary_file(output_file)

if __name__ == '__main__':
    main()
