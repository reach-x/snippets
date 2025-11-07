# StringUtils PowerShell Module
# Reusable string utility functions

<#
.SYNOPSIS
    Converts a string to title case (first letter of each word capitalized)
.EXAMPLE
    ConvertTo-TitleCase "hello world"
    # Returns: "Hello World"
#>
function ConvertTo-TitleCase {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        $textInfo = (Get-Culture).TextInfo
        return $textInfo.ToTitleCase($InputString.ToLower())
    }
}

<#
.SYNOPSIS
    Truncates a string to specified length and adds ellipsis
.EXAMPLE
    Limit-StringLength "This is a long string" -MaxLength 10
    # Returns: "This is..."
#>
function Limit-StringLength {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString,

        [Parameter(Mandatory=$true)]
        [int]$MaxLength,

        [string]$Suffix = "..."
    )

    process {
        if ($InputString.Length -le $MaxLength) {
            return $InputString
        }
        else {
            $truncateLength = $MaxLength - $Suffix.Length
            return $InputString.Substring(0, $truncateLength) + $Suffix
        }
    }
}

<#
.SYNOPSIS
    Converts a string to slug format (lowercase, hyphen-separated)
.EXAMPLE
    ConvertTo-Slug "Hello World! This is a test"
    # Returns: "hello-world-this-is-a-test"
#>
function ConvertTo-Slug {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        $slug = $InputString.ToLower()
        $slug = $slug -replace '[^\w\s-]', ''  # Remove special characters
        $slug = $slug -replace '\s+', '-'      # Replace spaces with hyphens
        $slug = $slug -replace '-+', '-'       # Replace multiple hyphens with single
        $slug = $slug.Trim('-')                # Trim hyphens from ends
        return $slug
    }
}

<#
.SYNOPSIS
    Checks if a string is a palindrome
.EXAMPLE
    Test-Palindrome "racecar"
    # Returns: True
#>
function Test-Palindrome {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        # Normalize: remove non-alphanumeric and convert to lowercase
        $normalized = ($InputString -replace '[^a-zA-Z0-9]', '').ToLower()
        $reversed = -join $normalized.ToCharArray()[($normalized.Length-1)..0]
        return $normalized -eq $reversed
    }
}

<#
.SYNOPSIS
    Counts occurrences of a substring in a string
.EXAMPLE
    Measure-SubstringOccurrence "hello world hello" -Substring "hello"
    # Returns: 2
#>
function Measure-SubstringOccurrence {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString,

        [Parameter(Mandatory=$true)]
        [string]$Substring
    )

    process {
        if ([string]::IsNullOrEmpty($Substring)) {
            return 0
        }

        $count = 0
        $position = 0

        while (($position = $InputString.IndexOf($Substring, $position)) -ne -1) {
            $count++
            $position += $Substring.Length
        }

        return $count
    }
}

<#
.SYNOPSIS
    Extracts all numbers from a string
.EXAMPLE
    Get-NumbersFromString "There are 123 apples and 45 oranges"
    # Returns: @("123", "45")
#>
function Get-NumbersFromString {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        return [regex]::Matches($InputString, '\d+') | ForEach-Object { $_.Value }
    }
}

<#
.SYNOPSIS
    Wraps text to specified width
.EXAMPLE
    Format-TextWrap "This is a long sentence" -Width 10
#>
function Format-TextWrap {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString,

        [Parameter(Mandatory=$true)]
        [int]$Width
    )

    process {
        $words = $InputString.Split(' ')
        $lines = @()
        $currentLine = ""

        foreach ($word in $words) {
            if (($currentLine.Length + $word.Length + 1) -le $Width) {
                if ($currentLine.Length -gt 0) {
                    $currentLine += " "
                }
                $currentLine += $word
            }
            else {
                if ($currentLine.Length -gt 0) {
                    $lines += $currentLine
                }
                $currentLine = $word
            }
        }

        if ($currentLine.Length -gt 0) {
            $lines += $currentLine
        }

        return $lines -join "`n"
    }
}

<#
.SYNOPSIS
    Removes consecutive duplicate characters
.EXAMPLE
    Remove-DuplicateChars "hellooo world"
    # Returns: "helo world"
#>
function Remove-DuplicateChars {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        $result = ""
        $lastChar = $null

        foreach ($char in $InputString.ToCharArray()) {
            if ($char -ne $lastChar) {
                $result += $char
                $lastChar = $char
            }
        }

        return $result
    }
}

<#
.SYNOPSIS
    Converts string to camelCase
.EXAMPLE
    ConvertTo-CamelCase "hello world example"
    # Returns: "helloWorldExample"
#>
function ConvertTo-CamelCase {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        $words = $InputString -split '[\s_-]+'
        if ($words.Count -eq 0) {
            return ""
        }

        $result = $words[0].ToLower()

        for ($i = 1; $i -lt $words.Count; $i++) {
            $word = $words[$i]
            if ($word.Length -gt 0) {
                $result += $word.Substring(0, 1).ToUpper() + $word.Substring(1).ToLower()
            }
        }

        return $result
    }
}

<#
.SYNOPSIS
    Converts string to snake_case
.EXAMPLE
    ConvertTo-SnakeCase "HelloWorldExample"
    # Returns: "hello_world_example"
#>
function ConvertTo-SnakeCase {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        # Insert underscore before capital letters
        $snake = $InputString -creplace '([A-Z])', '_$1'
        # Convert to lowercase
        $snake = $snake.ToLower()
        # Remove leading underscore
        $snake = $snake.TrimStart('_')
        # Replace spaces and hyphens with underscores
        $snake = $snake -replace '[\s-]+', '_'
        # Remove consecutive underscores
        $snake = $snake -replace '_+', '_'

        return $snake
    }
}

<#
.SYNOPSIS
    Reverses a string
.EXAMPLE
    Invoke-ReverseString "hello"
    # Returns: "olleh"
#>
function Invoke-ReverseString {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        return -join $InputString.ToCharArray()[($InputString.Length-1)..0]
    }
}

<#
.SYNOPSIS
    Checks if string contains only alphabetic characters
.EXAMPLE
    Test-AlphabeticString "hello"
    # Returns: True
#>
function Test-AlphabeticString {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        return $InputString -match '^[a-zA-Z]+$'
    }
}

<#
.SYNOPSIS
    Checks if string contains only numeric characters
.EXAMPLE
    Test-NumericString "12345"
    # Returns: True
#>
function Test-NumericString {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        return $InputString -match '^\d+$'
    }
}

<#
.SYNOPSIS
    Checks if string is a valid email address
.EXAMPLE
    Test-EmailAddress "test@example.com"
    # Returns: True
#>
function Test-EmailAddress {
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [string]$InputString
    )

    process {
        return $InputString -match '^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$'
    }
}

# Export module members
Export-ModuleMember -Function @(
    'ConvertTo-TitleCase',
    'Limit-StringLength',
    'ConvertTo-Slug',
    'Test-Palindrome',
    'Measure-SubstringOccurrence',
    'Get-NumbersFromString',
    'Format-TextWrap',
    'Remove-DuplicateChars',
    'ConvertTo-CamelCase',
    'ConvertTo-SnakeCase',
    'Invoke-ReverseString',
    'Test-AlphabeticString',
    'Test-NumericString',
    'Test-EmailAddress'
)

# Demo usage (only runs when module is imported)
if ($MyInvocation.InvocationName -ne '.') {
    Write-Host "=== StringUtils Module Demo ===`n"

    Write-Host "Title Case:"
    ConvertTo-TitleCase "hello world from powershell"

    Write-Host "`nTruncate:"
    Limit-StringLength "This is a very long string" -MaxLength 15

    Write-Host "`nSlug:"
    ConvertTo-Slug "Hello World! This is PowerShell"

    Write-Host "`nPalindrome:"
    Write-Host "  'racecar': $(Test-Palindrome 'racecar')"
    Write-Host "  'hello': $(Test-Palindrome 'hello')"

    Write-Host "`nCount occurrences:"
    Write-Host "  'hello' in 'hello world hello': $(Measure-SubstringOccurrence 'hello world hello' -Substring 'hello')"

    Write-Host "`nExtract numbers:"
    $numbers = Get-NumbersFromString "I have 42 apples and 17 oranges"
    Write-Host "  $($numbers -join ', ')"

    Write-Host "`nCamel case:"
    ConvertTo-CamelCase "hello world example"

    Write-Host "`nSnake case:"
    ConvertTo-SnakeCase "HelloWorldExample"

    Write-Host "`nReverse:"
    Invoke-ReverseString "hello"

    Write-Host "`nValidate email:"
    Write-Host "  'test@example.com': $(Test-EmailAddress 'test@example.com')"
    Write-Host "  'invalid.email': $(Test-EmailAddress 'invalid.email')"

    Write-Host "`n"
}
