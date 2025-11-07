#!/usr/bin/env lua

local text = "  Hello, World!  "

print(string.format("Original: '%s'", text))
print(string.format("Upper: '%s'", string.upper(text)))
print(string.format("Lower: '%s'", string.lower(text)))

-- Trim function (Lua doesn't have built-in trim)
local function trim(s)
    return s:match("^%s*(.-)%s*$")
end
print(string.format("Trim: '%s'", trim(text)))

print(string.format("Replace: '%s'", string.gsub(text, "World", "Lua")))
print(string.format("Length: %d", string.len(text)))
print(string.format("Substring: '%s'", string.sub(trim(text), 1, 5)))
print(string.format("Find: %s", string.find(text, "World") and "true" or "false"))
print(string.format("Repeat: '%s'", string.rep("-", 20)))
