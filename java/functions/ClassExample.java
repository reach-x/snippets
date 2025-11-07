class Person {
    protected String name;
    protected int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String greet() {
        return String.format("Hello, I'm %s and I'm %d years old", this.name, this.age);
    }

    public String birthday() {
        this.age++;
        return String.format("Happy birthday! Now %d years old", this.age);
    }

    @Override
    public String toString() {
        return String.format("Person(name=%s, age=%d)", this.name, this.age);
    }
}

class Employee extends Person {
    private String jobTitle;

    public Employee(String name, int age, String jobTitle) {
        super(name, age);
        this.jobTitle = jobTitle;
    }

    public String work() {
        return String.format("%s is working as a %s", this.name, this.jobTitle);
    }

    @Override
    public String toString() {
        return String.format("Employee(name=%s, age=%d, job=%s)", this.name, this.age, this.jobTitle);
    }
}

public class ClassExample {
    public static void main(String[] args) {
        Person person = new Person("Alice", 30);
        System.out.println(person.greet());
        System.out.println(person.birthday());
        System.out.println(person);

        Employee employee = new Employee("Bob", 25, "Software Engineer");
        System.out.println(employee.greet());
        System.out.println(employee.work());
        System.out.println(employee);
    }
}
