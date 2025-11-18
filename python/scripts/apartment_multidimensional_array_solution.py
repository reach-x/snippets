apartmentBuilding = [
    ["Apt 101", "Apt 102", "Apt 103"], 
    ["Apt 201", "Apt 202", "Apt 203"], 
    ["Apt 301", "Apt 302", "Apt 303"]
]

# TODO: Update "Apt 202" to "Renovated Apt 202" in `apartmentBuilding`
print("x" * 70)
print(" " * 70)
print("x" * 70)


for floor_index, floor in enumerate(apartmentBuilding):
    for unit_index, unit in enumerate(floor):
        print(unit + ", ", end='')
        if unit == "Apt 202":          
            apartmentBuilding[floor_index][unit_index] = "Renovated Apt 202"   


print(" " * 70)
print("x" * 70)
print(" " * 70)
print("x" * 70)
print(" " * 70)

# Print all the apartments after the renovation update
for floor in apartmentBuilding:
    for unit in floor:
        print(unit + ", ", end='')
    print()  # Move to the next floor
print("x" * 70)
print(" " * 70)
print("x" * 70)
