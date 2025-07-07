const std = @import("std");

const Person = struct { name: []const u8, age: i32, city: []const u8 };

const Stat = struct { city: []const u8, count: i32, avg_age: f64 };

pub fn main() !void {
    const people = [_]Person{
        .{ .name = "Alice", .age = 30, .city = "Paris" },
        .{ .name = "Bob", .age = 15, .city = "Hanoi" },
        .{ .name = "Charlie", .age = 65, .city = "Paris" },
        .{ .name = "Diana", .age = 45, .city = "Hanoi" },
        .{ .name = "Eve", .age = 70, .city = "Paris" },
        .{ .name = "Frank", .age = 22, .city = "Hanoi" },
    };

    var groups = std.StringHashMap(struct { sum: i32, count: i32 }).init(std.heap.page_allocator);
    defer groups.deinit();

    for (people) |p| {
        if (groups.getPtr(p.city)) |g| {
            g.sum += p.age;
            g.count += 1;
        } else {
            try groups.put(p.city, .{ .sum = p.age, .count = 1 });
        }
    }

    std.debug.print("--- People grouped by city ---\n", .{});
    var it = groups.iterator();
    while (it.next()) |kv| {
        const avg = @as(f64, @floatFromInt(kv.value.sum)) / @as(f64, @floatFromInt(kv.value.count));
        std.debug.print("{s}: count = {d}, avg_age = {d}\n", .{ kv.key, kv.value.count, avg });
    }
}
