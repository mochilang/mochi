const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| {
        sum += it;
    }
    return sum;
}

var nums: []const i32 = undefined;
var result: f64 = undefined;

pub fn main() void {
    nums = &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)) };
    result = blk0: {
        var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator);
        for (nums) |n| {
            if (!((n > @as(i32, @intCast(1))))) continue;
            _tmp0.append(_sum_int(n)) catch unreachable;
        }
        const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
        break :blk0 _tmp1;
    };
    std.debug.print("{any}\n", .{result});
}
