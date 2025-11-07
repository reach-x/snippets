// Array operations in Objective-C

#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSLog(@"\n=== Array Operations in Objective-C ===\n");

        // Create arrays
        NSArray *numbers = @[@1, @2, @3, @4, @5];
        NSArray *fruits = @[@"apple", @"banana", @"cherry"];

        NSLog(@"Numbers: %@", numbers);
        NSLog(@"Fruits: %@", fruits);

        // Array count
        NSLog(@"\nCount: %lu", (unsigned long)[numbers count]);

        // Access elements
        NSLog(@"First: %@", numbers[0]);
        NSLog(@"Last: %@", [numbers lastObject]);

        // Mutable array
        NSMutableArray *mutableNumbers = [numbers mutableCopy];
        [mutableNumbers addObject:@6];
        NSLog(@"\nAfter addObject: %@", mutableNumbers);

        // Contains
        BOOL contains3 = [numbers containsObject:@3];
        NSLog(@"\nContains 3: %@", contains3 ? @"YES" : @"NO");

        // Index of object
        NSUInteger index = [numbers indexOfObject:@3];
        NSLog(@"Index of 3: %lu", (unsigned long)index);

        // Enumeration
        NSLog(@"\nEnumeration:");
        for (NSNumber *num in numbers) {
            NSLog(@"  %@", num);
        }

        // Block enumeration
        [numbers enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
            NSLog(@"  Index %lu: %@", (unsigned long)idx, obj);
        }];

        // Map (using blocks)
        NSMutableArray *squared = [NSMutableArray array];
        for (NSNumber *num in numbers) {
            NSNumber *square = @([num intValue] * [num intValue]);
            [squared addObject:square];
        }
        NSLog(@"\nSquared: %@", squared);

        // Filter
        NSPredicate *predicate = [NSPredicate predicateWithBlock:^BOOL(id obj, NSDictionary *bindings) {
            return [obj intValue] % 2 == 0;
        }];
        NSArray *evens = [numbers filteredArrayUsingPredicate:predicate];
        NSLog(@"Evens: %@", evens);

        // Sort
        NSArray *unsorted = @[@5, @2, @8, @1, @9, @3];
        NSArray *sorted = [unsorted sortedArrayUsingSelector:@selector(compare:)];
        NSLog(@"\nSorted: %@", sorted);

        // Reverse
        NSArray *reversed = [[numbers reverseObjectEnumerator] allObjects];
        NSLog(@"Reversed: %@", reversed);
    }
    return 0;
}
