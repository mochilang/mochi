const std = @import("std");

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| {
        if (it == item) return true;
    }
    return false;
}

var xs: []const i32 = undefined;
var ys: []const i32 = undefined;
var m: std.AutoHashMap([]const u8, i32) = undefined;
var s: []const u8 = undefined;

pub fn main() void {
    xs = &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)) };
    ys = blk0: {
        var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator);
        for (xs) |x| {
            if (!((@mod(x, @as(i32, @intCast(2))) == @as(i32, @intCast(1))))) continue;
            _tmp0.append(x) catch unreachable;
        }
        const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
        break :blk0 _tmp1;
    };
    std.debug.print("{any}\n", .{_contains_list_int(ys, @as(i32, @intCast(1)))});
    std.debug.print("{any}\n", .{_contains_list_int(ys, @as(i32, @intCast(2)))});
    m = blk1: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("a", @as(i32, @intCast(1))) catch unreachable;
        break :blk1 m;
    };
    std.debug.print("{s}\n", .{m.contains("a")});
    std.debug.print("{s}\n", .{m.contains("b")});
    s = "hello";
    std.debug.print("{s}\n", .{_contains_list_int(s, "ell")});
    std.debug.print("{s}\n", .{_contains_list_int(s, "foo")});
}
