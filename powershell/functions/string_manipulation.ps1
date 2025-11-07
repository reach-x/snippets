# String manipulation examples in PowerShell

# String concatenation
$str1 = "Hello"
$str2 = "World"
$concatenated = $str1 + " " + $str2
Write-Host "Concatenated: $concatenated"

# String interpolation
$name = "Alice"
$greeting = "Hello, $name!"
Write-Host $greeting

# Here-string (multi-line)
$multiline = @"
This is a
multi-line
string
"@
Write-Host "`nMulti-line string:`n$multiline"

# String length
Write-Host "`nLength of greeting: $($greeting.Length)"

# Uppercase and lowercase
Write-Host "Uppercase: $($greeting.ToUpper())"
Write-Host "Lowercase: $($greeting.ToLower())"

# String contains
$text = "Hello, World!"
Write-Host "`nContains 'World': $($text.Contains('World'))"

# String starts with / ends with
Write-Host "Starts with 'Hello': $($text.StartsWith('Hello'))"
Write-Host "Ends with '!': $($text.EndsWith('!'))"

# String replacement
$replaced = $text.Replace("World", "PowerShell")
Write-Host "Replaced: $replaced"

# String splitting
$csv = "apple,banana,cherry"
$fruits = $csv.Split(',')
Write-Host "`nSplit by comma:"
$fruits | ForEach-Object { Write-Host "  $_" }

# String joining
$joined = $fruits -join ' | '
Write-Host "Joined with |: $joined"

# String trimming
$padded = "  hello  "
Write-Host "`nOriginal: '$padded'"
Write-Host "Trimmed: '$($padded.Trim())'"
Write-Host "Trim left: '$($padded.TrimStart())'"
Write-Host "Trim right: '$($padded.TrimEnd())'"

# Substring
$slice = $text.Substring(0, 5)
Write-Host "`nSubstring [0,5]: $slice"

# Index of
$index = $text.IndexOf("World")
Write-Host "Index of 'World': $index"

# String formatting
$formatted = "Name: {0}, Age: {1}, City: {2}" -f "Bob", 30, "New York"
Write-Host "`nFormatted: $formatted"

# Padding
$number = "42"
$padded = $number.PadLeft(5, '0')
Write-Host "`nPadded left: $padded"

# Regular expression matching
$email = "test@example.com"
if ($email -match '^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$') {
    Write-Host "`n'$email' is a valid email"
}

# Regular expression replacement
$phone = "123-456-7890"
$digitsOnly = $phone -replace '[^0-9]', ''
Write-Host "Digits only: $digitsOnly"

# Case-insensitive comparison
if ("Hello" -ieq "hello") {
    Write-Host "`nCase-insensitive match"
}

# String comparison operators
Write-Host "`nComparison operators:"
Write-Host "  -eq (equal): $('abc' -eq 'abc')"
Write-Host "  -ne (not equal): $('abc' -ne 'xyz')"
Write-Host "  -like (wildcard): $('hello world' -like '*world*')"
Write-Host "  -match (regex): $('test123' -match '\d+')"

# Remove characters
$withSpaces = "H e l l o"
$noSpaces = $withSpaces.Replace(" ", "")
Write-Host "`nRemove spaces: $noSpaces"

# Reverse string
$reversed = -join ($text.ToCharArray() | Sort-Object { -$_.GetHashCode() })
# Better way:
$reversed = -join ($text.ToCharArray()[($text.Length-1)..0])
Write-Host "Reversed: $reversed"

# Check if string is empty or null
$empty = ""
Write-Host "`nIs empty: $([string]::IsNullOrEmpty($empty))"
Write-Host "Is whitespace: $([string]::IsNullOrWhiteSpace('   '))"
