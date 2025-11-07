struct Person {
    name: String,
    age: u32,
}

impl Person {
    fn new(name: String, age: u32) -> Self {
        Person { name, age }
    }

    fn greet(&self) -> String {
        format!("Hello, I'm {} and I'm {} years old", self.name, self.age)
    }

    fn birthday(&mut self) -> String {
        self.age += 1;
        format!("Happy birthday! Now {} years old", self.age)
    }
}

struct Employee {
    person: Person,
    job_title: String,
}

impl Employee {
    fn new(name: String, age: u32, job_title: String) -> Self {
        Employee {
            person: Person::new(name, age),
            job_title,
        }
    }

    fn work(&self) -> String {
        format!("{} is working as a {}", self.person.name, self.job_title)
    }
}

fn main() {
    let mut person = Person::new(String::from("Alice"), 30);
    println!("{}", person.greet());
    println!("{}", person.birthday());
    println!("Person {{ name: {}, age: {} }}", person.name, person.age);

    let employee = Employee::new(
        String::from("Bob"),
        25,
        String::from("Software Engineer"),
    );
    println!("{}", employee.person.greet());
    println!("{}", employee.work());
}
