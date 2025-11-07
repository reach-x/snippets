// Async operations in Dart

import 'dart:async';

// Async function
Future<String> fetchData() async {
  await Future.delayed(Duration(seconds: 1));
  return 'Data loaded';
}

// Multiple async operations
Future<void> fetchMultiple() async {
  print('Fetching data...');

  var result1 = await fetchData();
  print('Result 1: $result1');

  var result2 = await fetchData();
  print('Result 2: $result2');
}

// Parallel async
Future<void> fetchParallel() async {
  print('\nFetching in parallel...');

  var results = await Future.wait([
    fetchData(),
    fetchData(),
    fetchData(),
  ]);

  print('All results: $results');
}

// Stream example
Stream<int> countStream(int max) async* {
  for (var i = 1; i <= max; i++) {
    await Future.delayed(Duration(milliseconds: 100));
    yield i;
  }
}

void main() async {
  print('\n=== Async Operations in Dart ===\n');

  await fetchMultiple();
  await fetchParallel();

  print('\n=== Stream ===\n');
  await for (var count in countStream(5)) {
    print('Count: $count');
  }

  print('\nDone!');
}
