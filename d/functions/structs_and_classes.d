#!/usr/bin/env rdmd
// Structs and classes in D

import std.stdio;

// Struct
struct Point
{
    double x, y;

    double distance()
    {
        import std.math : sqrt;
        return sqrt(x * x + y * y);
    }
}

// Class
class Person
{
    string name;
    int age;

    this(string name, int age)
    {
        this.name = name;
        this.age = age;
    }

    void greet()
    {
        writeln("Hello, I'm ", name, " and I'm ", age, " years old");
    }
}

// Generic struct
struct Box(T)
{
    T value;

    this(T val)
    {
        value = val;
    }

    T get()
    {
        return value;
    }
}

void main()
{
    writeln("\n=== Structs and Classes in D ===\n");

    // Struct
    auto p = Point(3.0, 4.0);
    writeln("Point: (", p.x, ", ", p.y, ")");
    writeln("Distance: ", p.distance());

    // Class
    auto person = new Person("Alice", 30);
    person.greet();

    // Generic struct
    auto intBox = Box!int(42);
    auto stringBox = Box!string("hello");
    writeln("\nInt box: ", intBox.get());
    writeln("String box: ", stringBox.get());
}
