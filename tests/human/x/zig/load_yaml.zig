const std = @import("std");

const Person = struct { name: []const u8, age: i32, email: []const u8 };

pub fn main() void {
    const people = [_]Person{
        .{ .name = "Alice", .age = 20, .email = "a@example.com" },
        .{ .name = "Bob", .age = 30, .email = "b@example.com" },
    };

    for (people) |p| if (p.age >= 18) {
        std.debug.print("{s} {s}\n", .{ p.name, p.email });
    }
}
