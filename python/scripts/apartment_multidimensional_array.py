# Creating a 2D array
array = [[1, 2, 3], 
         [4, 5, 6], 
         [7, 8, 9]]
print(array)


# Accessing an element
print(array[1][0])  # Outputs: 4

# Updating an element
array[0][1] = 'New Code'  
print(array)

# Finding the number of rows
num_floors = len(array)
print(num_floors) # Outputs: 3

# Adding a new row to our array
array.append(['Unit-1', 'Unit-2', 'Unit-3'])
print(array)

# Removing an element
array[1].remove('New Code')
print(array)

array = [["Apt 101", "Apt 102", "Apt 103"], 
         ["Apt 201", "Exit Floor", "Apt 203"], 
         ["Apt 301", "Apt 302", "Apt 303"]]
# Loop through 2D array
for floor in array:
    for unit in floor:
        print(unit, end =', ')
    print()
"""
Prints:
Apt 101, Apt 102, Apt 103, 
Apt 201, Exit Floor, Apt 203, 
Apt 301, Apt 302, Apt 303, 
"""

# Break in nested loop
for floor in array:
    for unit in floor:
        if unit == 'Exit Floor':
            break
        print(unit, end =', ')
    print()
"""
Prints:
Apt 101, Apt 102, Apt 103, 
Apt 201, 
Apt 301, Apt 302, Apt 303, 
"""

# Continue in nested loop
for floor in array:
    for unit in floor:
        if unit == 'Exit Floor':
            continue
        print(unit, end =', ')
    print()
"""
Prints:
Apt 101, Apt 102, Apt 103, 
Apt 201, Apt 203, 
Apt 301, Apt 302, Apt 303, 
"""


apartmentBuilding = [
    ["Apt 101", "Apt 102", "Apt 103"], 
    ["Apt 201", "Apt 202", "Apt 203"], 
    ["Apt 301", "Apt 302", "Apt 303"]
]

# TODO: Update "Apt 202" to "Renovated Apt 202" in `apartmentBuilding`

# Print all the apartments after the renovation update
for floor in apartmentBuilding:
    for unit in floor:
        print(unit + ", ", end='')
    print()  # Move to the next floor



