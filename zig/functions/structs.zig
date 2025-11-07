// Structs and functions in Zig

const std = @import("std");

const Point = struct {
    x: f32,
    y: f32,

    pub fn distance(self: Point) f32 {
        return @sqrt(self.x * self.x + self.y * self.y);
    }
};

const Rectangle = struct {
    width: f32,
    height: f32,

    pub fn area(self: Rectangle) f32 {
        return self.width * self.height;
    }
};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.print("\n=== Structs in Zig ===\n\n", .{});

    const p = Point{ .x = 3.0, .y = 4.0 };
    try stdout.print("Point: ({d}, {d})\n", .{ p.x, p.y });
    try stdout.print("Distance: {d:.2}\n", .{p.distance()});

    const rect = Rectangle{ .width = 5.0, .height = 3.0 };
    try stdout.print("\nRectangle: {d} x {d}\n", .{ rect.width, rect.height });
    try stdout.print("Area: {d:.2}\n", .{rect.area()});
}
