const std = @import("std");

const Person = struct { name: []const u8, age: i32 };

pub fn main() !void {
    const people = [_]Person{
        .{ .name = "Alice", .age = 30 },
        .{ .name = "Bob", .age = 25 },
    };

    var gpa = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();
    for (people) |p| {
        const j = try std.json.stringifyAlloc(gpa, p, .{});
        defer gpa.free(j);
        try stdout.writeAll(j);
        try stdout.writeByte('\n');
    }
}
