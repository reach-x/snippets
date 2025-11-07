#!/usr/bin/env pwsh
# System monitoring script for PowerShell
# Displays system information, CPU, memory, disk usage

# Function to format bytes
function Format-Bytes {
    param([long]$bytes)

    if ($bytes -ge 1TB) {
        return "{0:N2} TB" -f ($bytes / 1TB)
    }
    elseif ($bytes -ge 1GB) {
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

# Function to get OS information
function Get-OSInfo {
    Write-Host "`n=== Operating System Information ===" -ForegroundColor Cyan

    if ($IsWindows -or $env:OS -match "Windows") {
        $os = Get-CimInstance Win32_OperatingSystem
        Write-Host "OS: $($os.Caption)"
        Write-Host "Version: $($os.Version)"
        Write-Host "Architecture: $($os.OSArchitecture)"
        Write-Host "Computer Name: $($os.CSName)"
        Write-Host "Last Boot: $($os.LastBootUpTime)"

        $uptime = (Get-Date) - $os.LastBootUpTime
        Write-Host "Uptime: $($uptime.Days)d $($uptime.Hours)h $($uptime.Minutes)m"
    }
    else {
        # Unix-like systems
        Write-Host "OS: $(uname -s)"
        Write-Host "Version: $(uname -r)"
        Write-Host "Architecture: $(uname -m)"
        Write-Host "Computer Name: $(hostname)"

        # Try to get uptime on Unix
        if (Test-Path "/proc/uptime") {
            $uptimeSeconds = [int](Get-Content /proc/uptime).Split()[0]
            $uptime = [TimeSpan]::FromSeconds($uptimeSeconds)
            Write-Host "Uptime: $($uptime.Days)d $($uptime.Hours)h $($uptime.Minutes)m"
        }
    }
}

# Function to get CPU information
function Get-CPUInfo {
    Write-Host "`n=== CPU Information ===" -ForegroundColor Cyan

    if ($IsWindows -or $env:OS -match "Windows") {
        $cpu = Get-CimInstance Win32_Processor
        Write-Host "Name: $($cpu.Name)"
        Write-Host "Cores: $($cpu.NumberOfCores)"
        Write-Host "Logical Processors: $($cpu.NumberOfLogicalProcessors)"
        Write-Host "Max Speed: $($cpu.MaxClockSpeed) MHz"
        Write-Host "Current Load: $($cpu.LoadPercentage)%"
    }
    else {
        # Unix-like systems
        if ($IsMacOS) {
            $cpuBrand = sysctl -n machdep.cpu.brand_string
            $cpuCores = sysctl -n hw.physicalcpu
            $cpuLogical = sysctl -n hw.logicalcpu
            Write-Host "Name: $cpuBrand"
            Write-Host "Cores: $cpuCores"
            Write-Host "Logical Processors: $cpuLogical"
        }
        elseif ($IsLinux) {
            if (Test-Path "/proc/cpuinfo") {
                $cpuInfo = Get-Content /proc/cpuinfo
                $cpuModel = ($cpuInfo | Select-String "model name" | Select-Object -First 1) -replace ".*: ", ""
                $cpuCores = ($cpuInfo | Select-String "cpu cores" | Select-Object -First 1) -replace ".*: ", ""
                Write-Host "Name: $cpuModel"
                Write-Host "Cores: $cpuCores"
            }
        }
    }
}

# Function to get memory information
function Get-MemoryInfo {
    Write-Host "`n=== Memory Information ===" -ForegroundColor Cyan

    if ($IsWindows -or $env:OS -match "Windows") {
        $os = Get-CimInstance Win32_OperatingSystem
        $totalMem = $os.TotalVisibleMemorySize * 1KB
        $freeMem = $os.FreePhysicalMemory * 1KB
        $usedMem = $totalMem - $freeMem
        $percentUsed = [math]::Round(($usedMem / $totalMem) * 100, 2)

        Write-Host "Total: $(Format-Bytes $totalMem)"
        Write-Host "Used: $(Format-Bytes $usedMem) ($percentUsed%)"
        Write-Host "Free: $(Format-Bytes $freeMem)"
    }
    else {
        # Unix-like systems
        if ($IsMacOS) {
            $totalMem = [long](sysctl -n hw.memsize)
            Write-Host "Total: $(Format-Bytes $totalMem)"

            # Get memory usage from vm_stat
            $vmStat = vm_stat
            $pageSize = [long](sysctl -n hw.pagesize)
            # Parse vm_stat output (simplified)
            Write-Host "Memory stats available via: vm_stat"
        }
        elseif ($IsLinux -and (Test-Path "/proc/meminfo")) {
            $memInfo = Get-Content /proc/meminfo
            $totalMem = [long](($memInfo | Select-String "MemTotal") -replace ".*:\s+(\d+).*", '$1') * 1KB
            $availMem = [long](($memInfo | Select-String "MemAvailable") -replace ".*:\s+(\d+).*", '$1') * 1KB
            $usedMem = $totalMem - $availMem
            $percentUsed = [math]::Round(($usedMem / $totalMem) * 100, 2)

            Write-Host "Total: $(Format-Bytes $totalMem)"
            Write-Host "Used: $(Format-Bytes $usedMem) ($percentUsed%)"
            Write-Host "Available: $(Format-Bytes $availMem)"
        }
    }
}

# Function to get disk information
function Get-DiskInfo {
    Write-Host "`n=== Disk Information ===" -ForegroundColor Cyan

    if ($IsWindows -or $env:OS -match "Windows") {
        $disks = Get-CimInstance Win32_LogicalDisk -Filter "DriveType=3"
        foreach ($disk in $disks) {
            $percentUsed = [math]::Round((($disk.Size - $disk.FreeSpace) / $disk.Size) * 100, 2)
            Write-Host "`nDrive: $($disk.DeviceID)"
            Write-Host "  Volume: $($disk.VolumeName)"
            Write-Host "  Total: $(Format-Bytes $disk.Size)"
            Write-Host "  Free: $(Format-Bytes $disk.FreeSpace)"
            Write-Host "  Used: $percentUsed%"
        }
    }
    else {
        # Unix-like systems - use df
        $dfOutput = df -h | Select-Object -Skip 1
        foreach ($line in $dfOutput) {
            $parts = $line -split '\s+', 9
            if ($parts.Count -ge 6) {
                Write-Host "`nFilesystem: $($parts[0])"
                Write-Host "  Mount: $($parts[-1])"
                Write-Host "  Total: $($parts[1])"
                Write-Host "  Used: $($parts[2]) ($($parts[4]))"
                Write-Host "  Free: $($parts[3])"
            }
        }
    }
}

# Function to get process information
function Get-TopProcesses {
    param(
        [int]$count = 5
    )

    Write-Host "`n=== Top $count Processes by CPU ===" -ForegroundColor Cyan
    Get-Process | Sort-Object CPU -Descending | Select-Object -First $count |
        Format-Table Name, CPU, @{Name="Memory(MB)"; Expression={[math]::Round($_.WorkingSet / 1MB, 2)}} -AutoSize

    Write-Host "`n=== Top $count Processes by Memory ===" -ForegroundColor Cyan
    Get-Process | Sort-Object WorkingSet -Descending | Select-Object -First $count |
        Format-Table Name, CPU, @{Name="Memory(MB)"; Expression={[math]::Round($_.WorkingSet / 1MB, 2)}} -AutoSize
}

# Function to get network information
function Get-NetworkInfo {
    Write-Host "`n=== Network Information ===" -ForegroundColor Cyan

    if ($IsWindows -or $env:OS -match "Windows") {
        $adapters = Get-NetAdapter | Where-Object Status -eq "Up"
        foreach ($adapter in $adapters) {
            $ipInfo = Get-NetIPAddress -InterfaceIndex $adapter.ifIndex -AddressFamily IPv4 -ErrorAction SilentlyContinue
            Write-Host "`nAdapter: $($adapter.Name)"
            Write-Host "  Status: $($adapter.Status)"
            Write-Host "  Speed: $($adapter.LinkSpeed)"
            if ($ipInfo) {
                Write-Host "  IP: $($ipInfo.IPAddress)"
            }
        }
    }
    else {
        # Unix-like systems - use ifconfig or ip
        Write-Host "Network interfaces:"
        if (Get-Command ifconfig -ErrorAction SilentlyContinue) {
            ifconfig | Select-String "^\w+:|inet "
        }
        elseif (Get-Command ip -ErrorAction SilentlyContinue) {
            ip addr show | Select-String "^\d+:|inet "
        }
    }
}

# Main execution
function Show-SystemMonitor {
    Write-Host "=====================================" -ForegroundColor Green
    Write-Host "   System Monitoring Dashboard" -ForegroundColor Green
    Write-Host "=====================================" -ForegroundColor Green
    Write-Host "Time: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')"

    Get-OSInfo
    Get-CPUInfo
    Get-MemoryInfo
    Get-DiskInfo
    Get-TopProcesses -count 5
    Get-NetworkInfo

    Write-Host "`n=====================================" -ForegroundColor Green
    Write-Host "   Monitoring Complete" -ForegroundColor Green
    Write-Host "=====================================" -ForegroundColor Green
}

# Run the monitor
Show-SystemMonitor

# Optional: Continuous monitoring
<#
param(
    [int]$interval = 5  # seconds
)

while ($true) {
    Clear-Host
    Show-SystemMonitor
    Write-Host "`nRefreshing in $interval seconds... (Ctrl+C to stop)"
    Start-Sleep -Seconds $interval
}
#>
