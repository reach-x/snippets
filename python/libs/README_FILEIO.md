# FileIO - Python File I/O Library

A simple, intuitive Python library for common file operations. Makes working with files, JSON, CSV, and directories easy and consistent.

## Features

- ✅ Simple API for reading/writing files
- ✅ Built-in JSON support
- ✅ Built-in CSV support
- ✅ Directory operations
- ✅ Binary file support
- ✅ Automatic directory creation
- ✅ Temporary file management
- ✅ Comprehensive error handling
- ✅ Type hints for better IDE support

## Installation

Simply copy `fileio.py` into your project directory and import it:

```python
from fileio import FileIO, JSONFile, CSVFile, Directory
```

Or use the convenience functions:

```python
from fileio import read, write, read_json, write_json
```

## Quick Start

```python
from fileio import FileIO, JSONFile, CSVFile

# Read and write text files
FileIO.write('hello.txt', 'Hello, World!')
content = FileIO.read('hello.txt')

# Work with JSON
data = {'name': 'John', 'age': 30}
JSONFile.write('data.json', data)
loaded = JSONFile.read('data.json')

# Work with CSV
rows = [{'name': 'Alice', 'score': 95}, {'name': 'Bob', 'score': 87}]
CSVFile.write('scores.csv', rows)
data = CSVFile.read('scores.csv')
```

## API Reference

### FileIO Class

#### Reading Files

```python
# Read entire file as string
content = FileIO.read('file.txt')
content = FileIO.read('file.txt', encoding='utf-8')

# Read file as list of lines
lines = FileIO.read_lines('file.txt')
lines = FileIO.read_lines('file.txt', strip=True)  # Remove whitespace

# Read binary file
data = FileIO.read_binary('image.png')
```

#### Writing Files

```python
# Write string to file (overwrites)
FileIO.write('output.txt', 'Hello, World!')
FileIO.write('output.txt', content, encoding='utf-8')

# Write list of lines
FileIO.write_lines('output.txt', ['line 1', 'line 2', 'line 3'])

# Write binary data
FileIO.write_binary('output.bin', b'\x00\x01\x02')

# Append to file
FileIO.append('log.txt', 'New log entry\n')
```

#### File Operations

```python
# Check if file exists
if FileIO.exists('config.json'):
    print("Config found!")

# Delete file
FileIO.delete('temp.txt')
FileIO.delete('temp.txt', missing_ok=True)  # Don't error if missing

# Copy file
FileIO.copy('source.txt', 'backup.txt')
FileIO.copy('source.txt', 'backup.txt', overwrite=False)  # Error if exists

# Move/rename file
FileIO.move('old_name.txt', 'new_name.txt')

# Get file size
size = FileIO.size('data.txt')  # Returns bytes

# Get file extension
ext = FileIO.get_extension('document.pdf')  # Returns '.pdf'

# Get filename without path
name = FileIO.get_basename('/path/to/file.txt')  # Returns 'file.txt'
```

### JSONFile Class

```python
# Read JSON file
data = JSONFile.read('config.json')
data = JSONFile.read('config.json', encoding='utf-8')

# Write JSON file
data = {'key': 'value', 'number': 42}
JSONFile.write('output.json', data)
JSONFile.write('output.json', data, indent=4)  # Custom indentation
```

### CSVFile Class

```python
# Read CSV file (returns list of dictionaries)
rows = CSVFile.read('data.csv')
rows = CSVFile.read('data.csv', has_header=True)

# Example output: [{'name': 'Alice', 'age': '25'}, {'name': 'Bob', 'age': '30'}]

# Write CSV file
data = [
    {'name': 'Alice', 'age': 25, 'city': 'Boston'},
    {'name': 'Bob', 'age': 30, 'city': 'Chicago'}
]
CSVFile.write('output.csv', data)
```

### Directory Class

```python
# Create directory (and parent directories)
Directory.create('data/processed')
Directory.create('data/raw', exist_ok=True)

# Check if directory exists
if Directory.exists('data'):
    print("Data directory found!")

# List files in directory
files = Directory.list_files('data')
files = Directory.list_files('data', pattern='*.txt')
files = Directory.list_files('data', pattern='*.csv', recursive=True)

# Copy directory
Directory.copy('source_dir', 'backup_dir')

# Delete directory and all contents
Directory.delete('temp_dir')
Directory.delete('temp_dir', missing_ok=True)
```

### Temporary Files

```python
from fileio import temp_file

# Use temporary file (automatically deleted)
with temp_file(suffix='.txt') as filepath:
    FileIO.write(filepath, 'temporary data')
    content = FileIO.read(filepath)
    # File automatically deleted after context

# Keep temporary file
with temp_file(suffix='.json', delete=False) as filepath:
    JSONFile.write(filepath, {'temp': 'data'})
    # File persists after context
```

### Convenience Functions

```python
from fileio import read, write, read_json, write_json, read_csv, write_csv

# Shorthand for common operations
content = read('file.txt')
write('file.txt', content)

data = read_json('config.json')
write_json('config.json', data)

rows = read_csv('data.csv')
write_csv('data.csv', rows)
```

## Complete Examples

### Example 1: Log File Management

```python
from fileio import FileIO
from datetime import datetime

def log_message(message, logfile='app.log'):
    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    entry = f"[{timestamp}] {message}\n"
    FileIO.append(logfile, entry)

def read_logs(logfile='app.log'):
    if FileIO.exists(logfile):
        return FileIO.read_lines(logfile)
    return []

# Usage
log_message("Application started")
log_message("Processing data...")
log_message("Application finished")

logs = read_logs()
for log in logs:
    print(log)
```

### Example 2: Configuration Management

```python
from fileio import JSONFile

class Config:
    def __init__(self, config_file='config.json'):
        self.config_file = config_file
        self.data = self.load()
    
    def load(self):
        try:
            return JSONFile.read(self.config_file)
        except FileNotFoundError:
            return self.defaults()
    
    def save(self):
        JSONFile.write(self.config_file, self.data, indent=2)
    
    def defaults(self):
        return {
            'debug': False,
            'port': 8080,
            'host': 'localhost'
        }
    
    def get(self, key, default=None):
        return self.data.get(key, default)
    
    def set(self, key, value):
        self.data[key] = value
        self.save()

# Usage
config = Config()
config.set('debug', True)
print(f"Debug mode: {config.get('debug')}")
```

### Example 3: Data Processing Pipeline

```python
from fileio import FileIO, CSVFile, Directory

def process_csv_files(input_dir, output_dir):
    """Process all CSV files in directory."""
    
    # Create output directory
    Directory.create(output_dir)
    
    # Get all CSV files
    csv_files = Directory.list_files(input_dir, pattern='*.csv')
    
    for csv_file in csv_files:
        # Read CSV
        data = CSVFile.read(csv_file)
        
        # Process data (example: filter rows)
        processed = [row for row in data if int(row.get('age', 0)) >= 18]
        
        # Write to output
        output_file = f"{output_dir}/{FileIO.get_basename(csv_file)}"
        CSVFile.write(output_file, processed)
        
        print(f"Processed {csv_file} -> {output_file}")

# Usage
process_csv_files('raw_data', 'processed_data')
```

### Example 4: Backup System

```python
from fileio import FileIO, Directory, temp_file
import zipfile
from datetime import datetime

def backup_directory(source_dir, backup_dir='backups'):
    """Create a timestamped backup of a directory."""
    
    # Create backup directory
    Directory.create(backup_dir)
    
    # Generate backup filename
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    backup_name = f"backup_{timestamp}.zip"
    backup_path = f"{backup_dir}/{backup_name}"
    
    # Get all files
    files = Directory.list_files(source_dir, recursive=True)
    
    # Create zip archive
    with zipfile.ZipFile(backup_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        for file in files:
            zipf.write(file)
    
    print(f"Backup created: {backup_path}")
    print(f"Size: {FileIO.size(backup_path)} bytes")
    
    return backup_path

# Usage
backup_directory('important_data')
```

### Example 5: Data Migration

```python
from fileio import JSONFile, CSVFile

def json_to_csv(json_file, csv_file):
    """Convert JSON file to CSV."""
    
    # Read JSON
    data = JSONFile.read(json_file)
    
    # Convert to list if single object
    if isinstance(data, dict):
        data = [data]
    
    # Write CSV
    CSVFile.write(csv_file, data)
    print(f"Converted {json_file} -> {csv_file}")

def csv_to_json(csv_file, json_file):
    """Convert CSV file to JSON."""
    
    # Read CSV
    data = CSVFile.read(csv_file)
    
    # Write JSON
    JSONFile.write(json_file, data, indent=2)
    print(f"Converted {csv_file} -> {json_file}")

# Usage
json_to_csv('data.json', 'data.csv')
csv_to_json('data.csv', 'data_copy.json')
```

## Error Handling

The library raises standard Python exceptions:

```python
from fileio import FileIO

try:
    content = FileIO.read('missing.txt')
except FileNotFoundError as e:
    print(f"File not found: {e}")
except IOError as e:
    print(f"IO error: {e}")

try:
    data = JSONFile.read('invalid.json')
except ValueError as e:
    print(f"Invalid JSON: {e}")
```

## Best Practices

1. **Use `create_dirs=True`** (default) when writing to nested paths:
   ```python
   FileIO.write('data/processed/output.txt', content)  # Creates directories
   ```

2. **Use context managers for temporary files**:
   ```python
   with temp_file() as temp:
       FileIO.write(temp, 'temp data')
       # Automatically cleaned up
   ```

3. **Check file existence before operations**:
   ```python
   if FileIO.exists('config.json'):
       config = JSONFile.read('config.json')
   else:
       config = default_config()
   ```

4. **Use `missing_ok=True` for optional deletions**:
   ```python
   FileIO.delete('temp.txt', missing_ok=True)  # No error if missing
   ```

5. **Specify encoding for non-UTF8 files**:
   ```python
   content = FileIO.read('latin1.txt', encoding='latin-1')
   ```

## Requirements

- Python 3.6+
- No external dependencies (uses only standard library)

## License

MIT License - Feel free to use in your projects!

## Contributing

This is a simple utility library. Feel free to extend it for your needs!

## Support

For issues or questions, refer to the inline documentation in `fileio.py`.
