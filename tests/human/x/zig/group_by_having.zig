const std = @import("std");

const Person = struct { name: []const u8, city: []const u8 };

pub fn main() !void {
    const people = [_]Person{
        .{ .name = "Alice", .city = "Paris" },
        .{ .name = "Bob", .city = "Hanoi" },
        .{ .name = "Charlie", .city = "Paris" },
        .{ .name = "Diana", .city = "Hanoi" },
        .{ .name = "Eve", .city = "Paris" },
        .{ .name = "Frank", .city = "Hanoi" },
        .{ .name = "George", .city = "Paris" },
    };

    var counts = std.StringHashMap(i32).init(std.heap.page_allocator);
    defer counts.deinit();

    for (people) |p| {
        if (counts.getPtr(p.city)) |c| {
            c.* += 1;
        } else {
            try counts.put(p.city, 1);
        }
    }

    var it = counts.iterator();
    while (it.next()) |kv| {
        if (kv.value >= 4) {
            std.debug.print("{s}: {d}\n", .{ kv.key, kv.value });
        }
    }
}
