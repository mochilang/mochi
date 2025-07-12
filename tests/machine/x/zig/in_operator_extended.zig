const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

const xs = &[_]i32{
    1,
    2,
    3,
}; // []const i32
const ys = blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (xs) |x| { if (!((@mod(x, 2) == 1))) continue; _tmp0.append(x) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }; // []const i32
const M = struct { a: i32, };
const m = M{ .a = 1 }; // M
const s = "hello"; // []const u8

pub fn main() void {
    std.debug.print("{}\n", .{_contains_list_int(ys, 1)});
    std.debug.print("{}\n", .{_contains_list_int(ys, 2)});
    std.debug.print("{any}\n", .{_contains_list_int(m, "a")});
    std.debug.print("{any}\n", .{_contains_list_int(m, "b")});
    std.debug.print("{}\n", .{_contains_list_int(s, "ell")});
    std.debug.print("{}\n", .{_contains_list_int(s, "foo")});
}
