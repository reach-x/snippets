#!/usr/bin/env lua

-- Basic function
local function greet(name)
    return string.format("Hello, %s!", name)
end

print(greet("Alice"))

-- Function with multiple returns
local function divide(a, b)
    if b == 0 then
        return nil, "Division by zero"
    end
    return a / b, nil
end

local result, err = divide(10, 2)
if err then
    print("Error: " .. err)
else
    print(string.format("10 / 2 = %.2f", result))
end

-- Higher-order function
local function map(tbl, func)
    local result = {}
    for i, v in ipairs(tbl) do
        result[i] = func(v)
    end
    return result
end

local numbers = {1, 2, 3, 4, 5}
local squared = map(numbers, function(x) return x * x end)
print("\nSquared: " .. table.concat(squared, ", "))

-- Closure
local function counter()
    local count = 0
    return function()
        count = count + 1
        return count
    end
end

local c = counter()
print("\nCounter:")
print(c())  -- 1
print(c())  -- 2
print(c())  -- 3
