# File operations in PowerShell

Write-Host "=== File Operations in PowerShell ===`n"

# Get current directory
$currentDir = Get-Location
Write-Host "Current directory: $currentDir"

# Set paths
$tmpDir = Join-Path $currentDir ".." "tmp"
$testFile = Join-Path $tmpDir "test.txt"
$copyFile = Join-Path $tmpDir "test_copy.txt"

# Create directory if it doesn't exist
if (-not (Test-Path $tmpDir)) {
    New-Item -ItemType Directory -Path $tmpDir | Out-Null
    Write-Host "Created directory: $tmpDir"
}

# Write to file
$content = @"
Hello, PowerShell!
This is a test file.
Line 3
Line 4
Line 5
"@

Set-Content -Path $testFile -Value $content
Write-Host "`nCreated file: $testFile"

# Check if file exists
Write-Host "File exists: $(Test-Path $testFile)"

# Read file content
Write-Host "`n--- File Content ---"
$fileContent = Get-Content -Path $testFile
$fileContent | ForEach-Object { Write-Host $_ }

# Read file as single string
$fileText = Get-Content -Path $testFile -Raw
Write-Host "`n--- File as single string (length: $($fileText.Length)) ---"

# Read specific lines
Write-Host "`n--- First 2 lines ---"
Get-Content -Path $testFile -TotalCount 2

# Append to file
Add-Content -Path $testFile -Value "Appended line"
Write-Host "`nAppended line to file"

# Copy file
Copy-Item -Path $testFile -Destination $copyFile
Write-Host "Copied file to: $copyFile"

# Move/Rename file
$renamedFile = Join-Path $tmpDir "renamed.txt"
Move-Item -Path $copyFile -Destination $renamedFile -Force
Write-Host "Moved file to: $renamedFile"

# Get file info
$fileInfo = Get-Item $testFile
Write-Host "`n--- File Info ---"
Write-Host "Name: $($fileInfo.Name)"
Write-Host "Full path: $($fileInfo.FullName)"
Write-Host "Size: $($fileInfo.Length) bytes"
Write-Host "Created: $($fileInfo.CreationTime)"
Write-Host "Modified: $($fileInfo.LastWriteTime)"
Write-Host "Extension: $($fileInfo.Extension)"

# Get file size in human readable format
function Get-FileSize {
    param([long]$bytes)

    if ($bytes -ge 1GB) {
        return "{0:N2} GB" -f ($bytes / 1GB)
    }
    elseif ($bytes -ge 1MB) {
        return "{0:N2} MB" -f ($bytes / 1MB)
    }
    elseif ($bytes -ge 1KB) {
        return "{0:N2} KB" -f ($bytes / 1KB)
    }
    else {
        return "$bytes bytes"
    }
}

Write-Host "File size: $(Get-FileSize $fileInfo.Length)"

# List files in directory
Write-Host "`n--- Files in tmp directory ---"
Get-ChildItem -Path $tmpDir -File | ForEach-Object {
    Write-Host "  $($_.Name) - $(Get-FileSize $_.Length)"
}

# Filter files by extension
Write-Host "`n--- .txt files only ---"
Get-ChildItem -Path $tmpDir -Filter "*.txt" | ForEach-Object {
    Write-Host "  $($_.Name)"
}

# Search for files recursively
Write-Host "`n--- Search for .txt files (recursive) ---"
Get-ChildItem -Path (Split-Path $tmpDir) -Filter "*.txt" -Recurse -ErrorAction SilentlyContinue |
    Select-Object -First 5 |
    ForEach-Object {
        Write-Host "  $($_.FullName)"
    }

# Read file line by line (for large files)
Write-Host "`n--- Process each line ---"
Get-Content -Path $testFile | ForEach-Object {
    $line = $_
    Write-Host "Line: $line (Length: $($line.Length))"
}

# Count lines, words, characters
$lines = (Get-Content -Path $testFile).Count
$words = (Get-Content -Path $testFile | ForEach-Object { $_.Split(' ') }).Count
$chars = (Get-Content -Path $testFile -Raw).Length
Write-Host "`n--- File Statistics ---"
Write-Host "Lines: $lines"
Write-Host "Words: $words"
Write-Host "Characters: $chars"

# Search in file (grep equivalent)
Write-Host "`n--- Search for 'test' ---"
Get-Content -Path $testFile | Select-String -Pattern "test" | ForEach-Object {
    Write-Host "  Line $($_.LineNumber): $($_.Line)"
}

# Replace text in file
Write-Host "`n--- Replace text ---"
(Get-Content -Path $testFile) -replace 'test', 'demo' | Set-Content -Path $testFile
Write-Host "Replaced 'test' with 'demo'"

# Create temporary file
$tempFile = New-TemporaryFile
Write-Host "`nCreated temporary file: $($tempFile.FullName)"

# Working with CSV
$csvFile = Join-Path $tmpDir "data.csv"
$data = @(
    [PSCustomObject]@{ Name = "Alice"; Age = 30; City = "New York" }
    [PSCustomObject]@{ Name = "Bob"; Age = 25; City = "Los Angeles" }
    [PSCustomObject]@{ Name = "Charlie"; Age = 35; City = "Chicago" }
)

$data | Export-Csv -Path $csvFile -NoTypeInformation
Write-Host "`nExported data to CSV: $csvFile"

# Read CSV
Write-Host "`n--- Import CSV ---"
$importedData = Import-Csv -Path $csvFile
$importedData | Format-Table -AutoSize

# Working with JSON
$jsonFile = Join-Path $tmpDir "data.json"
$data | ConvertTo-Json | Set-Content -Path $jsonFile
Write-Host "Exported data to JSON: $jsonFile"

# Read JSON
Write-Host "`n--- Import JSON ---"
$jsonData = Get-Content -Path $jsonFile -Raw | ConvertFrom-Json
$jsonData | Format-Table -AutoSize

# Get directory size
function Get-DirectorySize {
    param([string]$path)

    $size = (Get-ChildItem -Path $path -Recurse -File -ErrorAction SilentlyContinue |
            Measure-Object -Property Length -Sum).Sum

    return Get-FileSize $size
}

Write-Host "`nDirectory size: $(Get-DirectorySize $tmpDir)"

# Delete file
Remove-Item -Path $tempFile -Force
Write-Host "`nDeleted temporary file"

# Delete renamed file
Remove-Item -Path $renamedFile -Force
Write-Host "Deleted renamed file"

# Create and delete directory
$testDir = Join-Path $tmpDir "test_subdir"
New-Item -ItemType Directory -Path $testDir | Out-Null
Write-Host "Created test directory: $testDir"

# Remove directory (must be empty or use -Recurse)
Remove-Item -Path $testDir -Force
Write-Host "Deleted test directory"

# Check permissions
$acl = Get-Acl -Path $testFile
Write-Host "`n--- File Permissions ---"
Write-Host "Owner: $($acl.Owner)"

# Set file attributes
$fileInfo = Get-Item $testFile
# Make file read-only
$fileInfo.Attributes = [System.IO.FileAttributes]::ReadOnly
Write-Host "`nSet file to read-only: $($fileInfo.Attributes)"

# Remove read-only
$fileInfo.Attributes = [System.IO.FileAttributes]::Normal
Write-Host "Removed read-only: $($fileInfo.Attributes)"

Write-Host "`nFile operations completed!"
