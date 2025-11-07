# Snippets Repository Context

## Current Session Date
2025-11-07

## Active Task
Creating a comprehensive multi-language code snippets repository with standardized structure across all programming languages. Each language directory must contain: functions/, scripts/, libs/, and tmp/ subdirectories with sample code demonstrating core language features.

## Task Status

### Completed
- [x] Audited existing language directories and structure
- [x] Created standardized directory structure for incomplete existing languages
- [x] Added 15 new programming language directories with full structure
- [x] Created comprehensive Python samples (10 functions, 5 scripts, 2 libraries)
- [x] Created comprehensive JavaScript samples (7 functions, 1 script, 1 library)
- [x] Started Java samples (4 functions, 1 script, 1 library)

### In Progress
- [ ] Complete Java samples
- [ ] Create samples for Go, Ruby, Rust, Shell, Lua
- [ ] Create samples for new languages: C, C++, C#, TypeScript, Perl, Swift, Kotlin, R, Elixir, Haskell, Scala, Clojure, Dart, Zig

### Pending
- [ ] Create test programs for each language to verify functionality
- [ ] Commit and push all changes to GitHub

## Directory Structure

### Existing Languages (Enhanced)
- **applescript/** - macOS automation scripts (has script files)
- **basic/** - BASIC language (has scripts/ and games/)
- **cloudflare/** - Cloudflare Workers (has redirects/)
- **gimp/** - GIMP scripts (has python-fu/ and script-fu/)
- **go/** - NOW HAS: functions/, scripts/, libs/, tmp/
- **haproxy/** - HAProxy config examples
- **javascript/** - NOW HAS: functions/, scripts/, libs/, tmp/
- **lua/** - NOW HAS: functions/, scripts/, libs/, tmp/
- **phantomjs/** - PhantomJS scripts (has bin/)
- **php/** - FULLY COMPLETE: functions/ (126 files), scripts/ (124 files), libs/, tmp/, games/, json/
- **python/** - NOW HAS: functions/, scripts/, libs/, tmp/ (plus venv/)
- **ruby/** - NOW HAS: functions/, scripts/, libs/, tmp/
- **rust/** - NOW HAS: functions/, scripts/, libs/, tmp/
- **shell/** - NOW HAS: functions/, scripts/, libs/, tmp/
- **sql/** - SQL scripts and examples
- **prompts/** - AI/LLM prompts collection

### New Languages Added (All with functions/, scripts/, libs/, tmp/)
- **java/** - Java samples
- **c/** - C language samples
- **cpp/** - C++ samples
- **csharp/** - C# samples
- **typescript/** - TypeScript samples
- **perl/** - Perl samples
- **swift/** - Swift samples
- **kotlin/** - Kotlin samples
- **r/** - R language samples
- **elixir/** - Elixir samples
- **haskell/** - Haskell samples
- **scala/** - Scala samples
- **clojure/** - Clojure samples
- **dart/** - Dart samples
- **zig/** - Zig samples

## Files Created This Session

### Python (/python/functions/)
1. string_manipulation.py - String operations (upper, lower, trim, replace, split, etc.)
2. list_operations.py - List/array operations (append, pop, map, filter, etc.)
3. dict_operations.py - Dictionary operations (keys, values, items, iteration)
4. file_operations.py - File I/O (read, write, append, check existence)
5. json_operations.py - JSON parsing and serialization
6. regex_operations.py - Regular expression patterns (email, phone extraction)
7. datetime_operations.py - Date/time manipulation and formatting
8. math_operations.py - Mathematical functions (sqrt, pow, trig, rounding)
9. http_request.py - HTTP client using requests library
10. class_example.py - OOP with Person and Employee classes

### Python (/python/scripts/)
1. web_scraper.py - Web scraping with BeautifulSoup
2. csv_processor.py - CSV file reading and writing
3. log_analyzer.py - Log file analysis with regex
4. file_organizer.py - Organize files by extension into categories
5. password_generator.py - Secure password generation with strength checking

### Python (/python/libs/)
1. string_utils.py - Reusable string utility class (clean_phone, snake_case, camel_case, truncate, slugify)
2. http_client.py - HTTP client wrapper class with GET/POST/PUT/DELETE methods

### JavaScript (/javascript/functions/)
1. string_manipulation.js - String operations
2. array_operations.js - Array methods (map, filter, reduce, spread)
3. object_operations.js - Object manipulation and iteration
4. file_operations.js - File system operations with fs module
5. json_operations.js - JSON handling
6. async_operations.js - Async/await patterns and promises
7. class_example.js - ES6 classes with inheritance

### JavaScript (/javascript/scripts/)
1. http_server.js - Simple HTTP REST API server with multiple endpoints

### JavaScript (/javascript/libs/)
1. string_utils.js - String utility module (exportable)

### Java (/java/functions/)
1. StringManipulation.java - String operations
2. ArrayOperations.java - ArrayList operations
3. FileOperations.java - File I/O with NIO
4. ClassExample.java - OOP with Person and Employee classes

### Java (/java/scripts/)
1. HttpClient.java - HTTP client using java.net.http

### Java (/java/libs/)
1. StringUtils.java - String utility methods

## Sample Code Patterns

Each language follows consistent patterns demonstrating:
- **String manipulation**: upper, lower, trim, replace, split, substring
- **Data structures**: arrays/lists, dictionaries/maps, operations
- **File I/O**: read, write, append, check existence
- **JSON**: parsing and serialization
- **HTTP**: client requests (GET/POST)
- **OOP**: classes, inheritance, methods
- **Utilities**: reusable functions for common tasks

## Engineering Standards Applied
- Following CLAUDE.md conventions: snake_case for methods/variables
- Clear, descriptive variable names (no single-letter vars)
- Comprehensive comments where needed
- Each file is executable (shebang lines added)
- Organized by functionality (functions vs scripts vs libs)

## Next Steps

### Immediate (Continue Current Task)
1. Complete remaining Java samples (add more functions and scripts)
2. Create Go samples (functions, scripts, libs)
3. Create Ruby samples (functions, scripts, libs)
4. Create Rust samples (functions, scripts, libs)
5. Create Shell samples (functions, scripts, libs)
6. Create Lua samples (functions, scripts, libs)

### New Languages to Populate
7. C - string manipulation, memory management, file I/O examples
8. C++ - STL containers, classes, templates
9. C# - LINQ, async/await, collections
10. TypeScript - types, interfaces, generics
11. Perl - regex, text processing, file operations
12. Swift - optionals, protocols, closures
13. Kotlin - null safety, extension functions, coroutines
14. R - data frames, statistical functions, plotting
15. Elixir - pattern matching, pipes, processes
16. Haskell - pure functions, monads, type classes
17. Scala - case classes, pattern matching, futures
18. Clojure - immutable data, sequences, macros
19. Dart - async, streams, collections
20. Zig - memory management, comptime, error handling

### Testing and Finalization
21. Create test programs in each language to verify all functions work
22. Ensure all files have proper permissions (chmod +x where needed)
23. Add README.md files where helpful (but only if explicitly useful)
24. Final git commit and push to GitHub

## Known Issues
- None currently

## Dependencies
- Python: requests, beautifulsoup4 (optional, scripts handle gracefully)
- JavaScript: Node.js built-in modules only
- Java: JDK 11+ for java.net.http.HttpClient

## Git Repository
- **Status**: Clean at session start
- **Branch**: master
- **Recent commits**: 172da98 "adding latest snippets"
- **All changes uncommitted** - will commit after completing significant progress

## Server Connections
None - this is a local code repository project

## Database Connections
None - no database used

## API Endpoints
None - sample code demonstrates API patterns but uses public APIs (GitHub API for examples)

## Environment Variables
None required

## Architecture Decisions
1. Each language gets four subdirectories: functions/, scripts/, libs/, tmp/
2. functions/ = small focused examples of specific language features
3. scripts/ = complete runnable programs solving real problems
4. libs/ = reusable library code/modules
5. tmp/ = temporary output directory for file operations
6. Standardized naming: snake_case for Python/PHP, camelCase for Java/JavaScript
7. Every file demonstrates best practices for that language
8. Comprehensive coverage: strings, arrays, objects, files, JSON, HTTP, OOP

## Last Updated
2025-11-07 before compaction - mid-task creating language samples
