const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

const nums = &[_]i32{
    1,
    2,
    3,
}; // []const i32
const letters = &[_][]const u8{
    "A",
    "B",
}; // []const []const u8
const pairs = blk0: { var _tmp0 = std.ArrayList(struct {
    n: i32,
    l: []const u8,
}).init(std.heap.page_allocator); for (nums) |n| { for (letters) |l| { if (!((@mod(n, 2) == 0))) continue; _tmp0.append(struct {
    n: i32,
    l: []const u8,
}{
    .n = n,
    .l = l,
}) catch |err| handleError(err); } } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- Even pairs ---\n", .{});
    for (pairs) |p| {
        std.debug.print("{any} {any}\n", .{p.n, p.l});
    }
}
