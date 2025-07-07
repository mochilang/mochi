const std = @import("std");

const Person = struct { name: []const u8, age: i32, status: []const u8 };

pub fn main() void {
    var people = [_]Person{
        .{ .name = "Alice", .age = 17, .status = "minor" },
        .{ .name = "Bob", .age = 25, .status = "unknown" },
        .{ .name = "Charlie", .age = 18, .status = "unknown" },
        .{ .name = "Diana", .age = 16, .status = "minor" },
    };

    for (&people) |*p| {
        if (p.age >= 18) {
            p.status = "adult";
            p.age += 1;
        }
    }

    const expected = [_]Person{
        .{ .name = "Alice", .age = 17, .status = "minor" },
        .{ .name = "Bob", .age = 26, .status = "adult" },
        .{ .name = "Charlie", .age = 19, .status = "adult" },
        .{ .name = "Diana", .age = 16, .status = "minor" },
    };

    var ok = true;
    for (people, 0..) |p, idx| {
        if (!(std.mem.eql(u8, p.name, expected[idx].name) and p.age == expected[idx].age and std.mem.eql(u8, p.status, expected[idx].status))) {
            ok = false;
        }
    }
    if (ok) std.debug.print("ok\n", .{});
}
