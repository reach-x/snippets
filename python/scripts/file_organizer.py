#!/usr/bin/env python3

import os
import shutil
from pathlib import Path

def organize_files(source_dir):
    extensions_map = {
        'Images': ['.jpg', '.jpeg', '.png', '.gif', '.bmp', '.svg'],
        'Documents': ['.pdf', '.doc', '.docx', '.txt', '.md'],
        'Videos': ['.mp4', '.avi', '.mkv', '.mov'],
        'Audio': ['.mp3', '.wav', '.flac', '.m4a'],
        'Archives': ['.zip', '.tar', '.gz', '.rar'],
        'Code': ['.py', '.js', '.php', '.java', '.c', '.cpp'],
    }

    source_path = Path(source_dir)

    if not source_path.exists():
        print(f"Directory not found: {source_dir}")
        return

    files_moved = 0

    for file_path in source_path.iterdir():
        if file_path.is_file():
            extension = file_path.suffix.lower()

            category = 'Others'
            for cat, exts in extensions_map.items():
                if extension in exts:
                    category = cat
                    break

            dest_dir = source_path / category
            dest_dir.mkdir(exist_ok=True)

            dest_path = dest_dir / file_path.name
            print(f"Moving: {file_path.name} -> {category}/")

            files_moved += 1

    print(f"\nOrganized {files_moved} files into categories")

if __name__ == '__main__':
    import sys
    directory = sys.argv[1] if len(sys.argv) > 1 else '../tmp'
    print(f"Organizing files in: {directory}\n")
    organize_files(directory)
