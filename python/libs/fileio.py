"""
FileIO - A simple, intuitive file I/O library for Python

This library provides easy-to-use functions for common file operations
including reading, writing, copying, and working with various file formats.

Author: JB
License: MIT
"""

import os
import json
import csv
import shutil
from pathlib import Path
from typing import List, Dict, Any, Optional, Union
from contextlib import contextmanager
import tempfile


class FileIO:
    """Main class for file I/O operations."""
    
    @staticmethod
    def read(filepath: str, encoding: str = 'utf-8') -> str:
        """
        Read entire file contents as string.
        
        Args:
            filepath: Path to the file
            encoding: Text encoding (default: utf-8)
            
        Returns:
            File contents as string
            
        Example:
            content = FileIO.read('example.txt')
        """
        try:
            with open(filepath, 'r', encoding=encoding) as f:
                return f.read()
        except FileNotFoundError:
            raise FileNotFoundError(f"File not found: {filepath}")
        except Exception as e:
            raise IOError(f"Error reading file {filepath}: {str(e)}")
    
    @staticmethod
    def read_lines(filepath: str, encoding: str = 'utf-8', strip: bool = True) -> List[str]:
        """
        Read file as list of lines.
        
        Args:
            filepath: Path to the file
            encoding: Text encoding (default: utf-8)
            strip: Remove leading/trailing whitespace (default: True)
            
        Returns:
            List of lines
            
        Example:
            lines = FileIO.read_lines('example.txt')
        """
        try:
            with open(filepath, 'r', encoding=encoding) as f:
                lines = f.readlines()
                return [line.strip() if strip else line for line in lines]
        except FileNotFoundError:
            raise FileNotFoundError(f"File not found: {filepath}")
        except Exception as e:
            raise IOError(f"Error reading file {filepath}: {str(e)}")
    
    @staticmethod
    def read_binary(filepath: str) -> bytes:
        """
        Read file in binary mode.
        
        Args:
            filepath: Path to the file
            
        Returns:
            File contents as bytes
            
        Example:
            data = FileIO.read_binary('image.png')
        """
        try:
            with open(filepath, 'rb') as f:
                return f.read()
        except FileNotFoundError:
            raise FileNotFoundError(f"File not found: {filepath}")
        except Exception as e:
            raise IOError(f"Error reading binary file {filepath}: {str(e)}")
    
    @staticmethod
    def write(filepath: str, content: str, encoding: str = 'utf-8', 
              create_dirs: bool = True) -> None:
        """
        Write string content to file (overwrites existing).
        
        Args:
            filepath: Path to the file
            content: Content to write
            encoding: Text encoding (default: utf-8)
            create_dirs: Create parent directories if they don't exist
            
        Example:
            FileIO.write('output.txt', 'Hello, World!')
        """
        try:
            if create_dirs:
                os.makedirs(os.path.dirname(filepath) or '.', exist_ok=True)
            
            with open(filepath, 'w', encoding=encoding) as f:
                f.write(content)
        except Exception as e:
            raise IOError(f"Error writing to file {filepath}: {str(e)}")
    
    @staticmethod
    def write_lines(filepath: str, lines: List[str], encoding: str = 'utf-8',
                   create_dirs: bool = True) -> None:
        """
        Write list of lines to file.
        
        Args:
            filepath: Path to the file
            lines: List of lines to write
            encoding: Text encoding (default: utf-8)
            create_dirs: Create parent directories if they don't exist
            
        Example:
            FileIO.write_lines('output.txt', ['line 1', 'line 2'])
        """
        content = '\n'.join(lines)
        if lines and not lines[-1].endswith('\n'):
            content += '\n'
        FileIO.write(filepath, content, encoding, create_dirs)
    
    @staticmethod
    def write_binary(filepath: str, data: bytes, create_dirs: bool = True) -> None:
        """
        Write binary data to file.
        
        Args:
            filepath: Path to the file
            data: Binary data to write
            create_dirs: Create parent directories if they don't exist
            
        Example:
            FileIO.write_binary('output.bin', b'\\x00\\x01\\x02')
        """
        try:
            if create_dirs:
                os.makedirs(os.path.dirname(filepath) or '.', exist_ok=True)
            
            with open(filepath, 'wb') as f:
                f.write(data)
        except Exception as e:
            raise IOError(f"Error writing binary to file {filepath}: {str(e)}")
    
    @staticmethod
    def append(filepath: str, content: str, encoding: str = 'utf-8') -> None:
        """
        Append content to file.
        
        Args:
            filepath: Path to the file
            content: Content to append
            encoding: Text encoding (default: utf-8)
            
        Example:
            FileIO.append('log.txt', 'New log entry\\n')
        """
        try:
            with open(filepath, 'a', encoding=encoding) as f:
                f.write(content)
        except Exception as e:
            raise IOError(f"Error appending to file {filepath}: {str(e)}")
    
    @staticmethod
    def exists(filepath: str) -> bool:
        """
        Check if file exists.
        
        Args:
            filepath: Path to check
            
        Returns:
            True if file exists, False otherwise
            
        Example:
            if FileIO.exists('config.json'):
                ...
        """
        return os.path.exists(filepath)
    
    @staticmethod
    def delete(filepath: str, missing_ok: bool = False) -> None:
        """
        Delete a file.
        
        Args:
            filepath: Path to the file
            missing_ok: If True, don't raise error if file doesn't exist
            
        Example:
            FileIO.delete('temp.txt')
        """
        try:
            os.remove(filepath)
        except FileNotFoundError:
            if not missing_ok:
                raise FileNotFoundError(f"File not found: {filepath}")
        except Exception as e:
            raise IOError(f"Error deleting file {filepath}: {str(e)}")
    
    @staticmethod
    def copy(src: str, dst: str, overwrite: bool = True) -> None:
        """
        Copy a file.
        
        Args:
            src: Source file path
            dst: Destination file path
            overwrite: If False, raise error if destination exists
            
        Example:
            FileIO.copy('original.txt', 'backup.txt')
        """
        try:
            if not overwrite and os.path.exists(dst):
                raise FileExistsError(f"Destination file already exists: {dst}")
            
            os.makedirs(os.path.dirname(dst) or '.', exist_ok=True)
            shutil.copy2(src, dst)
        except FileNotFoundError:
            raise FileNotFoundError(f"Source file not found: {src}")
        except Exception as e:
            raise IOError(f"Error copying file from {src} to {dst}: {str(e)}")
    
    @staticmethod
    def move(src: str, dst: str, overwrite: bool = True) -> None:
        """
        Move/rename a file.
        
        Args:
            src: Source file path
            dst: Destination file path
            overwrite: If False, raise error if destination exists
            
        Example:
            FileIO.move('old_name.txt', 'new_name.txt')
        """
        try:
            if not overwrite and os.path.exists(dst):
                raise FileExistsError(f"Destination file already exists: {dst}")
            
            os.makedirs(os.path.dirname(dst) or '.', exist_ok=True)
            shutil.move(src, dst)
        except FileNotFoundError:
            raise FileNotFoundError(f"Source file not found: {src}")
        except Exception as e:
            raise IOError(f"Error moving file from {src} to {dst}: {str(e)}")
    
    @staticmethod
    def size(filepath: str) -> int:
        """
        Get file size in bytes.
        
        Args:
            filepath: Path to the file
            
        Returns:
            File size in bytes
            
        Example:
            size = FileIO.size('data.txt')
        """
        try:
            return os.path.getsize(filepath)
        except FileNotFoundError:
            raise FileNotFoundError(f"File not found: {filepath}")
        except Exception as e:
            raise IOError(f"Error getting file size {filepath}: {str(e)}")
    
    @staticmethod
    def get_extension(filepath: str) -> str:
        """
        Get file extension (including dot).
        
        Args:
            filepath: Path to the file
            
        Returns:
            File extension (e.g., '.txt')
            
        Example:
            ext = FileIO.get_extension('document.pdf')  # Returns '.pdf'
        """
        return os.path.splitext(filepath)[1]
    
    @staticmethod
    def get_basename(filepath: str) -> str:
        """
        Get filename without directory path.
        
        Args:
            filepath: Path to the file
            
        Returns:
            Base filename
            
        Example:
            name = FileIO.get_basename('/path/to/file.txt')  # Returns 'file.txt'
        """
        return os.path.basename(filepath)


class JSONFile:
    """Convenience class for working with JSON files."""
    
    @staticmethod
    def read(filepath: str, encoding: str = 'utf-8') -> Any:
        """
        Read and parse JSON file.
        
        Args:
            filepath: Path to JSON file
            encoding: Text encoding (default: utf-8)
            
        Returns:
            Parsed JSON data
            
        Example:
            data = JSONFile.read('config.json')
        """
        try:
            content = FileIO.read(filepath, encoding)
            return json.loads(content)
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON in file {filepath}: {str(e)}")
    
    @staticmethod
    def write(filepath: str, data: Any, indent: int = 2, 
              encoding: str = 'utf-8', create_dirs: bool = True) -> None:
        """
        Write data to JSON file.
        
        Args:
            filepath: Path to JSON file
            data: Data to serialize
            indent: Indentation spaces (default: 2)
            encoding: Text encoding (default: utf-8)
            create_dirs: Create parent directories if they don't exist
            
        Example:
            JSONFile.write('data.json', {'key': 'value'})
        """
        try:
            content = json.dumps(data, indent=indent, ensure_ascii=False)
            FileIO.write(filepath, content, encoding, create_dirs)
        except TypeError as e:
            raise TypeError(f"Data is not JSON serializable: {str(e)}")


class CSVFile:
    """Convenience class for working with CSV files."""
    
    @staticmethod
    def read(filepath: str, encoding: str = 'utf-8', 
             has_header: bool = True) -> List[Dict[str, str]]:
        """
        Read CSV file as list of dictionaries.
        
        Args:
            filepath: Path to CSV file
            encoding: Text encoding (default: utf-8)
            has_header: First row is header (default: True)
            
        Returns:
            List of row dictionaries
            
        Example:
            rows = CSVFile.read('data.csv')
        """
        try:
            with open(filepath, 'r', encoding=encoding, newline='') as f:
                if has_header:
                    reader = csv.DictReader(f)
                    return list(reader)
                else:
                    reader = csv.reader(f)
                    return [{'col' + str(i): val for i, val in enumerate(row)} 
                            for row in reader]
        except FileNotFoundError:
            raise FileNotFoundError(f"File not found: {filepath}")
        except Exception as e:
            raise IOError(f"Error reading CSV file {filepath}: {str(e)}")
    
    @staticmethod
    def write(filepath: str, data: List[Dict[str, Any]], 
              encoding: str = 'utf-8', create_dirs: bool = True) -> None:
        """
        Write list of dictionaries to CSV file.
        
        Args:
            filepath: Path to CSV file
            data: List of row dictionaries
            encoding: Text encoding (default: utf-8)
            create_dirs: Create parent directories if they don't exist
            
        Example:
            CSVFile.write('output.csv', [{'name': 'John', 'age': 30}])
        """
        if not data:
            raise ValueError("Cannot write empty data to CSV")
        
        try:
            if create_dirs:
                os.makedirs(os.path.dirname(filepath) or '.', exist_ok=True)
            
            with open(filepath, 'w', encoding=encoding, newline='') as f:
                fieldnames = list(data[0].keys())
                writer = csv.DictWriter(f, fieldnames=fieldnames)
                writer.writeheader()
                writer.writerows(data)
        except Exception as e:
            raise IOError(f"Error writing CSV file {filepath}: {str(e)}")


class Directory:
    """Class for directory operations."""
    
    @staticmethod
    def create(dirpath: str, exist_ok: bool = True) -> None:
        """
        Create a directory (and parent directories).
        
        Args:
            dirpath: Path to directory
            exist_ok: If True, don't raise error if directory exists
            
        Example:
            Directory.create('data/processed')
        """
        try:
            os.makedirs(dirpath, exist_ok=exist_ok)
        except Exception as e:
            raise IOError(f"Error creating directory {dirpath}: {str(e)}")
    
    @staticmethod
    def exists(dirpath: str) -> bool:
        """
        Check if directory exists.
        
        Args:
            dirpath: Path to check
            
        Returns:
            True if directory exists, False otherwise
        """
        return os.path.isdir(dirpath)
    
    @staticmethod
    def list_files(dirpath: str, pattern: str = '*', 
                   recursive: bool = False) -> List[str]:
        """
        List files in directory.
        
        Args:
            dirpath: Path to directory
            pattern: Glob pattern (default: '*' for all files)
            recursive: Search subdirectories (default: False)
            
        Returns:
            List of file paths
            
        Example:
            files = Directory.list_files('data', '*.txt')
        """
        path = Path(dirpath)
        if recursive:
            return [str(p) for p in path.rglob(pattern) if p.is_file()]
        else:
            return [str(p) for p in path.glob(pattern) if p.is_file()]
    
    @staticmethod
    def delete(dirpath: str, missing_ok: bool = False) -> None:
        """
        Delete a directory and all its contents.
        
        Args:
            dirpath: Path to directory
            missing_ok: If True, don't raise error if directory doesn't exist
            
        Example:
            Directory.delete('temp_data')
        """
        try:
            shutil.rmtree(dirpath)
        except FileNotFoundError:
            if not missing_ok:
                raise FileNotFoundError(f"Directory not found: {dirpath}")
        except Exception as e:
            raise IOError(f"Error deleting directory {dirpath}: {str(e)}")
    
    @staticmethod
    def copy(src: str, dst: str) -> None:
        """
        Copy entire directory tree.
        
        Args:
            src: Source directory
            dst: Destination directory
            
        Example:
            Directory.copy('data', 'data_backup')
        """
        try:
            shutil.copytree(src, dst)
        except FileNotFoundError:
            raise FileNotFoundError(f"Source directory not found: {src}")
        except FileExistsError:
            raise FileExistsError(f"Destination directory already exists: {dst}")
        except Exception as e:
            raise IOError(f"Error copying directory from {src} to {dst}: {str(e)}")


@contextmanager
def temp_file(suffix: str = '', prefix: str = 'tmp', 
              text: bool = True, delete: bool = True):
    """
    Context manager for creating temporary files.
    
    Args:
        suffix: File suffix/extension
        prefix: File prefix
        text: Open in text mode (default: True)
        delete: Delete file when done (default: True)
        
    Yields:
        File path
        
    Example:
        with temp_file(suffix='.txt') as filepath:
            FileIO.write(filepath, 'temporary data')
            # File automatically deleted after context
    """
    mode = 'w+t' if text else 'w+b'
    fd, filepath = tempfile.mkstemp(suffix=suffix, prefix=prefix, text=text)
    
    try:
        os.close(fd)
        yield filepath
    finally:
        if delete and os.path.exists(filepath):
            os.remove(filepath)


# Convenience functions at module level
def read(filepath: str, encoding: str = 'utf-8') -> str:
    """Convenience function for FileIO.read()"""
    return FileIO.read(filepath, encoding)


def write(filepath: str, content: str, encoding: str = 'utf-8') -> None:
    """Convenience function for FileIO.write()"""
    return FileIO.write(filepath, content, encoding)


def read_json(filepath: str) -> Any:
    """Convenience function for JSONFile.read()"""
    return JSONFile.read(filepath)


def write_json(filepath: str, data: Any, indent: int = 2) -> None:
    """Convenience function for JSONFile.write()"""
    return JSONFile.write(filepath, data, indent)


def read_csv(filepath: str) -> List[Dict[str, str]]:
    """Convenience function for CSVFile.read()"""
    return CSVFile.read(filepath)


def write_csv(filepath: str, data: List[Dict[str, Any]]) -> None:
    """Convenience function for CSVFile.write()"""
    return CSVFile.write(filepath, data)


if __name__ == '__main__':
    # Example usage and tests
    print("FileIO Library - Example Usage\n")
    
    # Text file operations
    print("1. Text file operations:")
    FileIO.write('example.txt', 'Hello, World!\nThis is a test.')
    content = FileIO.read('example.txt')
    print(f"   Read: {repr(content)}")
    
    FileIO.append('example.txt', '\nAppended line.')
    lines = FileIO.read_lines('example.txt')
    print(f"   Lines: {lines}")
    
    # JSON operations
    print("\n2. JSON operations:")
    data = {'name': 'John', 'age': 30, 'city': 'New York'}
    JSONFile.write('example.json', data)
    loaded = JSONFile.read('example.json')
    print(f"   Loaded: {loaded}")
    
    # CSV operations
    print("\n3. CSV operations:")
    csv_data = [
        {'name': 'Alice', 'age': '25', 'city': 'Boston'},
        {'name': 'Bob', 'age': '30', 'city': 'Chicago'}
    ]
    CSVFile.write('example.csv', csv_data)
    loaded_csv = CSVFile.read('example.csv')
    print(f"   Loaded: {loaded_csv}")
    
    # File info
    print("\n4. File information:")
    print(f"   Size: {FileIO.size('example.txt')} bytes")
    print(f"   Extension: {FileIO.get_extension('example.json')}")
    print(f"   Exists: {FileIO.exists('example.txt')}")
    
    # Directory operations
    print("\n5. Directory operations:")
    Directory.create('test_dir/subdir')
    FileIO.write('test_dir/file1.txt', 'File 1')
    FileIO.write('test_dir/subdir/file2.txt', 'File 2')
    files = Directory.list_files('test_dir', recursive=True)
    print(f"   Files in test_dir: {files}")
    
    # Cleanup
    print("\n6. Cleanup:")
    FileIO.delete('example.txt')
    FileIO.delete('example.json')
    FileIO.delete('example.csv')
    Directory.delete('test_dir')
    print("   All test files cleaned up!")
    
    # Temp file example
    print("\n7. Temporary file:")
    with temp_file(suffix='.txt') as temp_path:
        FileIO.write(temp_path, 'Temporary data')
        print(f"   Created temp file: {temp_path}")
        print(f"   Content: {FileIO.read(temp_path)}")
    print("   Temp file automatically deleted!")
