#!/usr/bin/awk -f
# Text processing in AWK

BEGIN {
    print ""
    print "=== Text Processing in AWK ==="
    print ""

    # Create sample data
    data[1] = "Alice 30 NYC"
    data[2] = "Bob 25 LA"
    data[3] = "Charlie 35 Chicago"

    # Process data
    for (i = 1; i <= 3; i++) {
        split(data[i], fields, " ")
        name = fields[1]
        age = fields[2]
        city = fields[3]

        printf "Name: %s, Age: %d, City: %s\n", name, age, city
    }

    # String operations
    print "\n=== String Operations ==="
    text = "Hello, World!"
    print "Original: " text
    print "Length: " length(text)
    print "Uppercase: " toupper(text)
    print "Lowercase: " tolower(text)
    print "Substring [1,5]: " substr(text, 1, 5)

    # Array operations
    print "\n=== Array Operations ==="
    numbers[1] = 10
    numbers[2] = 20
    numbers[3] = 30

    sum = 0
    for (i in numbers) {
        sum += numbers[i]
    }
    print "Sum: " sum

    # Pattern matching
    print "\n=== Pattern Matching ==="
    if (match(text, /World/)) {
        print "Found 'World' at position " RSTART
    }

    # Field splitting
    print "\n=== Field Processing ==="
    line = "apple:banana:cherry"
    n = split(line, fruits, ":")
    for (i = 1; i <= n; i++) {
        print i ": " fruits[i]
    }
}
