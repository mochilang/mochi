const std = @import("std");

const Person = struct {
    name: []const u8,
    age: i32,
};

const Adult = struct {
    name: []const u8,
    age: i32,
    is_senior: bool,
};

pub fn main() !void {
    const people = [_]Person{
        .{ .name = "Alice", .age = 30 },
        .{ .name = "Bob", .age = 15 },
        .{ .name = "Charlie", .age = 65 },
        .{ .name = "Diana", .age = 45 },
    };

    var adults = std.ArrayList(Adult).init(std.heap.page_allocator);
    defer adults.deinit();

    for (people) |p| {
        if (p.age >= 18) {
            try adults.append(.{ .name = p.name, .age = p.age, .is_senior = p.age >= 60 });
        }
    }

    std.debug.print("--- Adults ---\n", .{});
    for (adults.items) |person| {
        const note = if (person.is_senior) " (senior)" else "";
        std.debug.print("{s} is {d}{s}\n", .{ person.name, person.age, note });
    }
}

