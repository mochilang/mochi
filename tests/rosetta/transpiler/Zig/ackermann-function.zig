// Generated by Mochi Zig transpiler on 2025-07-24 20:52 +0700
const std = @import("std");

fn ackermann(m: i64, n: i64) i64 {
    if (m == 0) {
        return n + 1;
    }
    if (n == 0) {
        return ackermann(m - 1, 1);
    }
    return ackermann(m - 1, ackermann(m, n - 1));
}

fn mochi_main() void {
    std.io.getStdOut().writer().print("{s}\n", .{_concat_string("A(0, 0) = ", _str(ackermann(0, 0)))}) catch unreachable;
    std.io.getStdOut().writer().print("{s}\n", .{_concat_string("A(1, 2) = ", _str(ackermann(1, 2)))}) catch unreachable;
    std.io.getStdOut().writer().print("{s}\n", .{_concat_string("A(2, 4) = ", _str(ackermann(2, 4)))}) catch unreachable;
    std.io.getStdOut().writer().print("{s}\n", .{_concat_string("A(3, 4) = ", _str(ackermann(3, 4)))}) catch unreachable;
}

pub fn main() void {
    mochi_main();
}

fn _str(v: anytype) []const u8 {
    if (@TypeOf(v) == f64 or @TypeOf(v) == f32) {
        return std.fmt.allocPrint(std.heap.page_allocator, "{d:.1}", .{v}) catch unreachable;
    }
    return std.fmt.allocPrint(std.heap.page_allocator, "{any}", .{v}) catch unreachable;
}

fn _concat_string(a: []const u8, b: []const u8) []const u8 {
    const alloc = std.heap.page_allocator;
    return std.mem.concat(alloc, u8, &[_][]const u8{ a, b }) catch unreachable;
}
