const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.page_allocator;
    const obj = struct { a: i32, b: i32 }{ .a = 1, .b = 2 };
    const json = try std.json.stringifyAlloc(gpa, obj, .{});
    defer gpa.free(json);
    try std.io.getStdOut().writeAll(json);
    try std.io.getStdOut().writeByte('\n');
}
