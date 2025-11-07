// Array operations in Haxe

class ArrayOperations {
    static function main() {
        trace('\n=== Array Operations in Haxe ===\n');

        // Create arrays
        var numbers:Array<Int> = [1, 2, 3, 4, 5];
        var fruits:Array<String> = ['apple', 'banana', 'cherry'];

        trace('Numbers: ' + numbers);
        trace('Fruits: ' + fruits);

        // Array length
        trace('\nLength: ' + numbers.length);

        // Access elements
        trace('First: ' + numbers[0]);
        trace('Last: ' + numbers[numbers.length - 1]);

        // Add elements
        numbers.push(6);
        trace('\nAfter push(6): ' + numbers);

        numbers.unshift(0);
        trace('After unshift(0): ' + numbers);

        // Remove elements
        numbers.pop();
        trace('After pop(): ' + numbers);

        // Contains
        trace('\nContains 3: ' + numbers.indexOf(3) != -1);

        // Map
        var squared = numbers.map(function(x) return x * x);
        trace('\nMap (square): ' + squared);

        // Filter
        var evens = numbers.filter(function(x) return x % 2 == 0);
        trace('Filter (even): ' + evens);

        // Iteration
        trace('\nIteration:');
        for (num in numbers) {
            trace('  ' + num);
        }

        // Sort
        var unsorted = [5, 2, 8, 1, 9, 3];
        unsorted.sort(function(a, b) return a - b);
        trace('\nSorted: ' + unsorted);

        // Reverse
        var copy = numbers.copy();
        copy.reverse();
        trace('Reversed: ' + copy);

        // Join
        trace('Join: ' + numbers.join(', '));

        // Slice
        trace('\nSlice [1..4]: ' + numbers.slice(1, 4));

        // Concat
        var combined = numbers.concat([10, 11, 12]);
        trace('Concat: ' + combined);
    }
}
