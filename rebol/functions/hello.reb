REBOL [Title: "Hello World"]

print "Hello from REBOL!"

numbers: [1 2 3 4 5]
foreach num numbers [print num]

factorial: func [n] [
    either n <= 1 [1] [n * factorial n - 1]
]

print factorial 5
