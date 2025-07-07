const std = @import("std");

fn contains(slice: []const i32, value: i32) bool {
    return std.mem.indexOfScalar(i32, slice, value) != null;
}

pub fn main() !void {
    const a = [_]i32{1, 2};
    const b = [_]i32{2, 3};

    var buf_union: [4]i32 = undefined;
    var len_union: usize = 0;
    for (a) |x| {
        buf_union[len_union] = x;
        len_union += 1;
    }
    for (b) |y| {
        if (!contains(buf_union[0..len_union], y)) {
            buf_union[len_union] = y;
            len_union += 1;
        }
    }
    std.debug.print("{any}\n", .{buf_union[0..len_union]});

    var buf_except: [3]i32 = undefined;
    var len_except: usize = 0;
    for ([_]i32{1,2,3}) |x| {
        if (x != 2) {
            buf_except[len_except] = x;
            len_except += 1;
        }
    }
    std.debug.print("{any}\n", .{buf_except[0..len_except]});

    var buf_inter: [3]i32 = undefined;
    var len_inter: usize = 0;
    for ([_]i32{1,2,3}) |x| {
        if (contains(b[0..], x)) {
            buf_inter[len_inter] = x;
            len_inter += 1;
        }
    }
    std.debug.print("{any}\n", .{buf_inter[0..len_inter]});

    const union_all_len = a.len + b.len; // [1,2] union all [2,3]
    std.debug.print("{}\n", .{union_all_len});
}
