const std = @import("std");

var nums: []const i32 = undefined;
var letters: []const []const u8 = undefined;
var pairs: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    nums = &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)) };
    letters = &[_][]const u8{ "A", "B" };
    pairs = blk1: {
        var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (nums) |n| {
            for (letters) |l| {
                if (!((@mod(n, @as(i32, @intCast(2))) == @as(i32, @intCast(0))))) continue;
                _tmp0.append(blk0: {
                    var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                    m.put(n, n) catch unreachable;
                    m.put(l, l) catch unreachable;
                    break :blk0 m;
                }) catch unreachable;
            }
        }
        const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
        break :blk1 _tmp1;
    };
    std.debug.print("{s}\n", .{"--- Even pairs ---"});
    for (pairs) |p| {
        std.debug.print("{any} {any}\n", .{ p.n, p.l });
    }
}
