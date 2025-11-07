fn main() {
    let mut numbers = vec![1, 2, 3, 4, 5];

    println!("Numbers: {:?}", numbers);
    println!("Length: {}", numbers.len());

    let sum: i32 = numbers.iter().sum();
    println!("Sum: {}", sum);

    let max = numbers.iter().max();
    println!("Max: {:?}", max);

    let min = numbers.iter().min();
    println!("Min: {:?}", min);

    numbers.push(6);
    println!("After push: {:?}", numbers);

    numbers.pop();
    println!("After pop: {:?}", numbers);

    let squared: Vec<i32> = numbers.iter().map(|x| x * x).collect();
    println!("Squared: {:?}", squared);

    let evens: Vec<i32> = numbers.iter().filter(|x| *x % 2 == 0).copied().collect();
    println!("Even numbers: {:?}", evens);
}
