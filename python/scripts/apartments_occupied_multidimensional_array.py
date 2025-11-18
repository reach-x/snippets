building = [
    [("101", True), ("102", False), ("103", False)],
    [("201", True), ("202", True), ("203", False)]
]

for floor in building:
	for apt in floor:
		if apt[1] == True:
			print(apt[0] + " is occupied")
			break


# TODO: Create a multidimensional list representing an apartment building with 3 floors
# and 3 apartment units on each floor.
apartments = [
    ['apt 101', 'apt 102', 'apt 103'],
    ['apt 201', 'apt 202', 'apt 203'],
    ['apt 301', 'apt 302', 'apt 303'],
]
# TODO: Update the name of one of the apartments to "Vacant".
for floor_index, floor in enumerate(apartments):
    for unit_index, unit in enumerate(floor):
        if unit == "apt 101":
            apartments[floor_index][unit_index] = "apt 101 is vacant"
                        
# TODO: Print out all the apartment names in the building, one floor at a time.
for floor_index, floor in enumerate(apartments):
    print(f"Floor: ", {floor_index + 1})
    for unit in floor:
        print(unit)
    
    print("\n")
    

                        

