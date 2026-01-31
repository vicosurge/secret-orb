import os
import json

# Constants
JSON_FILE = "structure.json"

# Verify if file exists, if not then create it
def build_structure_file():
	if not os.path.exists("structure.json"):
		data = {"room_id": 0, "rooms": []}
		with open(JSON_FILE, "w") as json_file:
			json.dump(data, json_file, indent=4)
		print("Created",JSON_FILE,"file")

# Destroy the mad brute
def destroy_structure_file():
	if os.path.exists("structure.json"):
		os.remove(JSON_FILE)
	print("Deleted",JSON_FILE,"file")

# Save data to file
def save_structure_file(data):
	with open(JSON_FILE, "w") as json_file:
		json.dump(data, json_file, indent=4)
	print("Saved",JSON_FILE,"file")


# Add room function currently does not pass anything
# in the future it may need to reference other things
def add_room(data):
	print("Start building a room")

	print("Create rooms")
	room_name = input("Room Name: ")
	room_description = input("Room Description: ")
	n = input("North: ")
	if n == "": n = -1
	s = input("South: ")
	if s == "": s = -1
	e = input("East: ")
	if e == "": e = -1
	w = input("West: ")
	if w == "": w = -1
	u = input("Up: ")
	if u == "": u = -1
	d = input("Down: ")
	if d == "": d = -1
	room_lit = input("Room lit? [Y/N] ")
	if room_lit.lower() == "y":
		room_lit = True
	elif room_lit.lower() == "n":
		room_lit = False
	room_trigger = input("Room ID Trigger: ")
	if room_trigger == "": room_trigger = -1
	room_object = input("Object ID in Room: ")
	if room_object == "": room_object = -1
	room_npc = input("NPC ID in Room: ")
	if room_npc == "": room_npc = -1

	room = {
		"id": data["room_id"] + 1,
		"name": room_name,
		"description": room_description,
		"exits": {"north":n,"south": s,"east": e,"west": w,"up": u,"down": d},
		"lit": room_lit,
		"trigger": room_trigger,
		"npc": room_npc,
		"object": room_object,
		"visited": False,
		"tags": ["safe_zone"],
		"hazards": [],
		"fixtures": [],
		"event_flags": []
		}

	data["rooms"].append(room)
	data["room_id"] = data["room_id"] + 1
	save_structure_file(data)
	print("Added room,", room_name)
	print("*"*20)
	return data

def edit_room_field(room_field, room_field_data):
	print("The field",room_field,"contains the following content:",room_field_data)
	changed_field = input("Change to what? [NOTE: If you do not want to change it just press enter] ")
	if changed_field == "":
		return None
	else:
		return changed_field

def edit_room(data):
	print("Start editing a room")
	print("*"*20)
	print("NOTE: The numbers shown below are relative to the JSON dictionary, their ID is independent of this")
	for item in data["rooms"]:
		print("ID:",item["id"]-1,"| Room Name:",item["name"])
	print("Send 'q' to exit this menu")
	print("*"*20)
	room_number = input("Choose room ID to edit: ")
	try:
		if room_number.lower() == "q":
			return data
		room_number = int(room_number)
		room = data["rooms"][room_number]
		room_option = ""
		while room_option.lower() != "q":
			print("ID:",room["id"])
			print("Name:",room["name"])
			print("Description:",room["description"])
			print("North:",room["exits"]["north"])
			print("South:",room["exits"]["south"])
			print("East:",room["exits"]["east"])
			print("West:",room["exits"]["west"])
			print("Up:",room["exits"]["up"])
			print("Down:",room["exits"]["down"])
			print("Lit:",room["lit"])
			print("*"*20)
			room_option = input("Which option do you want to modify, type the name of the field to modify: ")
			try:
				print("*"*20)
				if room_option.lower() == "id":
					print("ID field cannot be modified for the purpose of maintaining continuity in the data")
					print("*"*20)
				if room_option.lower() == "name":
					returned_content = edit_room_field("name",room["name"])
					if returned_content is None:
						print("Modification of field cancelled")
						print("*"*20)
						pass
					else:
						room["name"] = returned_content
						print("Modified field",room["name"])
						print("*"*20)
				if room_option.lower() == "description":
					returned_content = edit_room_field("description",room["description"])
					if returned_content is None:
						print("Modification of field cancelled")
						print("*"*20)
						pass
					else:
						room["description"] = returned_content
						print("Modified field",room["description"])
						print("*"*20)
				if room_option.lower() == "q":
					save_changes = input("Save changes? [Y/N] ")
					if save_changes.lower() == "y":
						data["rooms"][room_number] = room
						save_structure_file(data)
						return data
					else:
						return data
			except Exception as e:
				print("*"*20)
				print("Incorrect option")
				print(e)
				pass
	except Exception as e:
		print("Incorrect option")
		print(e)
		pass

	return data

def main():

	build_structure_file()
	# Load all data in the structure file
	with open(JSON_FILE, "r") as json_file:
		data = json.load(json_file)

	choose = ""
	print("Welcome to the Structure Builder.")
	print("This script is a tool to help you build the structure for a text-adventure")
	while choose.lower() != "q":
		print("Choose one of the options below to begin your work")
		print("1) Add Room\n2) Edit Room\n3) Add Item\n4) Edit Item\n5) Add NPC\n6) Edit NPC")
		print("*"*20)
		print("R)ebuild Structure file\nQ)uit")
		print("*"*20)
		choose = input("Option: ")
		try:
			choose = int(choose)
			if choose == 1:
				data = add_room(data)
			if choose == 2:
				data = edit_room(data)
			choose = ""
		except Exception as e:
			if choose.lower() == "q":
				exit()
			if choose.lower() == "r":
				destroy_structure_file()
				build_structure_file()
			else:
				print("Incorrect option.\n")
				print(e)


if __name__ == "__main__":
	main()
