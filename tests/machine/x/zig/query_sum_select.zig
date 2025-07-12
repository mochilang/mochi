const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

const nums = &[_]i32{
    1,
    2,
    3,
}; // []const i32
const result = blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (nums) |n| { if (!((n > 1))) continue; _tmp0.append(_sum_int(n)) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; }; // f64

pub fn main() void {
    std.debug.print("{any}\n", .{result});
}
