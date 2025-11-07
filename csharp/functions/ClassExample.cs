using System;

class Person
{
    public string Name { get; set; }
    public int Age { get; set; }

    public Person(string name, int age)
    {
        Name = name;
        Age = age;
    }

    public string Greet()
    {
        return $"Hello, I'm {Name} and I'm {Age} years old";
    }

    public void Birthday()
    {
        Age++;
        Console.WriteLine($"Happy birthday! Now {Age} years old");
    }

    public override string ToString()
    {
        return $"Person(name={Name}, age={Age})";
    }
}

class Employee : Person
{
    public string JobTitle { get; set; }

    public Employee(string name, int age, string jobTitle) : base(name, age)
    {
        JobTitle = jobTitle;
    }

    public string Work()
    {
        return $"{Name} is working as a {JobTitle}";
    }

    public override string ToString()
    {
        return $"Employee(name={Name}, age={Age}, job={JobTitle})";
    }
}

class ClassExample
{
    static void Main()
    {
        Person person = new Person("Alice", 30);
        Console.WriteLine(person.Greet());
        person.Birthday();
        Console.WriteLine(person);

        Console.WriteLine();

        Employee employee = new Employee("Bob", 25, "Software Engineer");
        Console.WriteLine(employee.Greet());
        Console.WriteLine(employee.Work());
        Console.WriteLine(employee);
    }
}
