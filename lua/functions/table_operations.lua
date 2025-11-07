#!/usr/bin/env lua

-- Array-like table
local numbers = {1, 2, 3, 4, 5}

print("Numbers:")
for i, v in ipairs(numbers) do
    print(string.format("  [%d] = %d", i, v))
end

table.insert(numbers, 6)
print("\nAfter insert: " .. table.concat(numbers, ", "))

table.remove(numbers)
print("After remove: " .. table.concat(numbers, ", "))

-- Dictionary-like table
local person = {
    name = "John",
    age = 30,
    city = "New York"
}

print("\nPerson:")
for k, v in pairs(person) do
    print(string.format("  %s = %s", k, tostring(v)))
end

person.email = "john@example.com"
print("\nAfter adding email:")
for k, v in pairs(person) do
    print(string.format("  %s = %s", k, tostring(v)))
end

-- Table length
print(string.format("\nArray length: %d", #numbers))
