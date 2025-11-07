# Array operations in CoffeeScript

console.log '\n=== Array Operations in CoffeeScript ===\n'

# Create arrays
numbers = [1, 2, 3, 4, 5]
fruits = ['apple', 'banana', 'cherry']

console.log "Numbers: #{numbers}"
console.log "Fruits: #{fruits}"

# Array length
console.log "\nLength: #{numbers.length}"

# Access elements
console.log "First: #{numbers[0]}"
console.log "Last: #{numbers[numbers.length - 1]}"

# Add elements
numbers.push 6
console.log "\nAfter push(6): #{numbers}"

# Map
squared = (x * x for x in numbers)
console.log "\nMap (square): #{squared}"

# Filter
evens = (x for x in numbers when x % 2 is 0)
console.log "Filter (even): #{evens}"

# Reduce
sum = numbers.reduce (a, b) -> a + b
console.log "\nReduce (sum): #{sum}"

# Ranges
range = [1..10]
console.log "\nRange [1..10]: #{range}"

# Comprehensions
squares = (x * x for x in [1..5])
console.log "Squares: #{squares}"

# Destructuring
[first, second, ...rest] = numbers
console.log "\nDestructure:"
console.log "  First: #{first}"
console.log "  Second: #{second}"
console.log "  Rest: #{rest}"

# Object
person =
  name: 'Alice'
  age: 30
  greet: -> console.log "Hello, I'm #{@name}"

console.log "\nPerson: #{JSON.stringify(person)}"
person.greet()
