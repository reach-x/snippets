# Array and collection operations in PowerShell

Write-Host "=== Array Operations in PowerShell ===`n"

# Create arrays
$numbers = @(1, 2, 3, 4, 5)
$fruits = @("apple", "banana", "cherry")
$mixed = @(1, "two", 3.0, $true)

Write-Host "Numbers: $($numbers -join ', ')"
Write-Host "Fruits: $($fruits -join ', ')"

# Array length
Write-Host "`nArray length: $($numbers.Length)"
Write-Host "Array count: $($numbers.Count)"

# Access elements
Write-Host "`nFirst element: $($numbers[0])"
Write-Host "Last element: $($numbers[-1])"
Write-Host "Second to last: $($numbers[-2])"

# Array slicing
Write-Host "`nFirst 3 elements: $($numbers[0..2] -join ', ')"
Write-Host "Last 2 elements: $($numbers[-2..-1] -join ', ')"

# Add elements
$numbers += 6
Write-Host "`nAfter adding 6: $($numbers -join ', ')"

# Add multiple elements
$numbers += @(7, 8, 9)
Write-Host "After adding 7,8,9: $($numbers -join ', ')"

# Prepend element
$numbers = @(0) + $numbers
Write-Host "After prepending 0: $($numbers -join ', ')"

# Remove element (by creating new array)
$numbers = $numbers | Where-Object { $_ -ne 5 }
Write-Host "After removing 5: $($numbers -join ', ')"

# Contains check
Write-Host "`nContains 3: $($numbers -contains 3)"
Write-Host "Contains 100: $($numbers -contains 100)"

# Find index
$index = [array]::IndexOf($numbers, 4)
Write-Host "Index of 4: $index"

# Sort array
$unsorted = @(5, 2, 8, 1, 9, 3)
$sorted = $unsorted | Sort-Object
Write-Host "`nUnsorted: $($unsorted -join ', ')"
Write-Host "Sorted: $($sorted -join ', ')"

# Sort descending
$sortedDesc = $unsorted | Sort-Object -Descending
Write-Host "Sorted descending: $($sortedDesc -join ', ')"

# Reverse array
[array]::Reverse($sorted)
Write-Host "Reversed: $($sorted -join ', ')"

# ForEach loop
Write-Host "`nForEach loop:"
foreach ($num in @(1, 2, 3)) {
    Write-Host "  Number: $num"
}

# ForEach-Object (pipeline)
Write-Host "`nForEach-Object (squared):"
@(1, 2, 3, 4, 5) | ForEach-Object {
    Write-Host "  $_ squared = $($_ * $_)"
}

# Where-Object (filter)
$evens = @(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) | Where-Object { $_ % 2 -eq 0 }
Write-Host "`nEven numbers: $($evens -join ', ')"

# Select-Object (map)
$doubled = @(1, 2, 3, 4, 5) | ForEach-Object { $_ * 2 }
Write-Host "Doubled: $($doubled -join ', ')"

# Measure-Object (aggregate)
$stats = @(10, 20, 30, 40, 50) | Measure-Object -Sum -Average -Maximum -Minimum
Write-Host "`nStatistics:"
Write-Host "  Count: $($stats.Count)"
Write-Host "  Sum: $($stats.Sum)"
Write-Host "  Average: $($stats.Average)"
Write-Host "  Min: $($stats.Minimum)"
Write-Host "  Max: $($stats.Maximum)"

# Group-Object
$items = @("apple", "banana", "apricot", "blueberry", "cherry", "cantaloupe")
$grouped = $items | Group-Object { $_.Substring(0, 1) }
Write-Host "`nGrouped by first letter:"
$grouped | ForEach-Object {
    Write-Host "  $($_.Name): $($_.Count) items - $($_.Group -join ', ')"
}

# Select-Object (unique)
$duplicates = @(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
$unique = $duplicates | Select-Object -Unique
Write-Host "`nOriginal with duplicates: $($duplicates -join ', ')"
Write-Host "Unique: $($unique -join ', ')"

# Join arrays
$arr1 = @(1, 2, 3)
$arr2 = @(4, 5, 6)
$joined = $arr1 + $arr2
Write-Host "`nJoined arrays: $($joined -join ', ')"

# Array comparison
$diff = Compare-Object -ReferenceObject @(1, 2, 3) -DifferenceObject @(2, 3, 4)
Write-Host "`nArray differences:"
$diff | ForEach-Object {
    Write-Host "  $($_.InputObject) - $($_.SideIndicator)"
}

# ArrayList (dynamic)
$arrayList = New-Object System.Collections.ArrayList
[void]$arrayList.Add("first")
[void]$arrayList.Add("second")
[void]$arrayList.Add("third")
Write-Host "`nArrayList: $($arrayList -join ', ')"
$arrayList.Remove("second")
Write-Host "After removing 'second': $($arrayList -join ', ')"

# Hash tables (dictionaries)
Write-Host "`n=== Hash Tables ==="
$person = @{
    Name = "John Doe"
    Age = 30
    City = "New York"
}

Write-Host "Hash table:"
$person.GetEnumerator() | ForEach-Object {
    Write-Host "  $($_.Key): $($_.Value)"
}

# Access hash table values
Write-Host "`nName: $($person['Name'])"
Write-Host "Age: $($person.Age)"

# Add key-value pair
$person['Email'] = 'john@example.com'
Write-Host "Added email: $($person['Email'])"

# Check if key exists
Write-Host "`nContains 'Name' key: $($person.ContainsKey('Name'))"
Write-Host "Contains 'Phone' key: $($person.ContainsKey('Phone'))"

# Get all keys
Write-Host "`nAll keys: $($person.Keys -join ', ')"

# Get all values
Write-Host "All values: $($person.Values -join ', ')"

# Range operator
$range = 1..10
Write-Host "`nRange 1..10: $($range -join ', ')"

# Multidimensional arrays
$matrix = @(
    @(1, 2, 3),
    @(4, 5, 6),
    @(7, 8, 9)
)
Write-Host "`nMatrix [1][1]: $($matrix[1][1])"

# Array of objects
$employees = @(
    [PSCustomObject]@{ Name = "Alice"; Department = "IT"; Salary = 75000 }
    [PSCustomObject]@{ Name = "Bob"; Department = "HR"; Salary = 65000 }
    [PSCustomObject]@{ Name = "Charlie"; Department = "IT"; Salary = 80000 }
)

Write-Host "`nEmployees:"
$employees | Format-Table -AutoSize

# Filter objects
$itEmployees = $employees | Where-Object { $_.Department -eq "IT" }
Write-Host "IT employees:"
$itEmployees | Format-Table -AutoSize

# Calculate average salary
$avgSalary = ($employees | Measure-Object -Property Salary -Average).Average
Write-Host "Average salary: $avgSalary"
