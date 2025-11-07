// List operations in Dart

void main() {
  print('\n=== List Operations in Dart ===\n');

  // Create lists
  var numbers = [1, 2, 3, 4, 5];
  var fruits = ['apple', 'banana', 'cherry'];

  print('Numbers: $numbers');
  print('Fruits: $fruits');

  // List length
  print('\nLength: ${numbers.length}');

  // Access elements
  print('First: ${numbers.first}');
  print('Last: ${numbers.last}');
  print('Element at 2: ${numbers[2]}');

  // Add elements
  numbers.add(6);
  print('\nAfter add(6): $numbers');

  numbers.addAll([7, 8, 9]);
  print('After addAll: $numbers');

  // Insert
  numbers.insert(0, 0);
  print('After insert(0, 0): $numbers');

  // Remove
  numbers.remove(5);
  print('After remove(5): $numbers');

  // Contains
  print('\nContains 3: ${numbers.contains(3)}');
  print('Contains 100: ${numbers.contains(100)}');

  // Index of
  print('IndexOf 4: ${numbers.indexOf(4)}');

  // Sublist
  print('\nSublist [1, 4): ${numbers.sublist(1, 4)}');

  // Map
  var squared = numbers.map((x) => x * x).toList();
  print('\nMap (square): $squared');

  // Where (filter)
  var evens = numbers.where((x) => x % 2 == 0).toList();
  print('Filter (even): $evens');

  // Reduce
  var sum = numbers.reduce((a, b) => a + b);
  print('\nReduce (sum): $sum');

  // Fold
  var product = numbers.fold(1, (a, b) => a * b);
  print('Fold (product): $product');

  // Any and every
  print('\nAny > 5: ${numbers.any((x) => x > 5)}');
  print('Every > 0: ${numbers.every((x) => x > 0)}');

  // Sort
  var unsorted = [5, 2, 8, 1, 9, 3];
  unsorted.sort();
  print('\nSorted: $unsorted');

  // Reverse
  print('Reversed: ${numbers.reversed.toList()}');

  // Take and skip
  print('\nTake 3: ${numbers.take(3).toList()}');
  print('Skip 2: ${numbers.skip(2).toList()}');

  // List comprehension
  var squares = [for (var x in [1, 2, 3, 4, 5]) x * x];
  print('\nList comprehension: $squares');

  // Spread operator
  var combined = [...numbers, ...squares];
  print('Spread: $combined');
}
