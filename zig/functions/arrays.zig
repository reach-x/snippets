// Array operations in Zig

const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.print("\n=== Array Operations in Zig ===\n\n", .{});

    // Fixed-size arrays
    const numbers = [_]i32{ 1, 2, 3, 4, 5 };
    try stdout.print("Numbers: ", .{});
    for (numbers) |num| {
        try stdout.print("{} ", .{num});
    }
    try stdout.print("\n", .{});

    // Array length
    try stdout.print("\nLength: {}\n", .{numbers.len});

    // Access elements
    try stdout.print("First: {}\n", .{numbers[0]});
    try stdout.print("Last: {}\n", .{numbers[numbers.len - 1]});

    // Array iteration
    try stdout.print("\nSquares: ", .{});
    for (numbers) |num| {
        try stdout.print("{} ", .{num * num});
    }
    try stdout.print("\n", .{});

    // Sum array
    var sum: i32 = 0;
    for (numbers) |num| {
        sum += num;
    }
    try stdout.print("\nSum: {}\n", .{sum});

    // Find maximum
    var max = numbers[0];
    for (numbers[1..]) |num| {
        if (num > max) {
            max = num;
        }
    }
    try stdout.print("Max: {}\n", .{max});

    // Slices
    const slice = numbers[1..4];
    try stdout.print("\nSlice [1..4]: ", .{});
    for (slice) |num| {
        try stdout.print("{} ", .{num});
    }
    try stdout.print("\n", .{});
}
