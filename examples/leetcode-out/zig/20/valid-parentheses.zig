const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _slice_list(comptime T: type, v: []const T, start: i32, end: i32, step: i32) []T {
    var s = start;
    var e = end;
    var st = step;
    const n: i32 = @as(i32, @intCast(v.len));
    if (s < 0) s += n;
    if (e < 0) e += n;
    if (st == 0) st = 1;
    if (s < 0) s = 0;
    if (e > n) e = n;
    if (st > 0 and e < s) e = s;
    if (st < 0 and e > s) e = s;
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    var i: i32 = s;
    while ((st > 0 and i < e) or (st < 0 and i > e)) : (i += st) {
        res.append(v[@as(usize, @intCast(i))]) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

fn isValid(s: []const u8) bool {
    var stack = std.ArrayList(u8).init(std.heap.page_allocator);
    const n: i32 = (s).len;
    for (@as(i32,@intCast(0)) .. n) |i| {
        const c: i32 = s[i];
        if (std.mem.eql(u8, c, "(")) {
            try stack.append(@as(i32,@intCast(")")));
        } else         if (std.mem.eql(u8, c, "[")) {
            try stack.append(@as(i32,@intCast("]")));
        } else         if (std.mem.eql(u8, c, "{")) {
            try stack.append(@as(i32,@intCast("}")));
        } else {
            if (((stack).len == @as(i32,@intCast(0)))) {
                return false;
            }
            const top: []const u8 = stack[((stack).len - @as(i32,@intCast(1)))];
            if (!std.mem.eql(u8, top, c)) {
                return false;
            }
            stack = _slice_list([]const u8, stack, @as(i32,@intCast(0)), ((stack).len - @as(i32,@intCast(1))), 1);
        }
    }
    return ((stack).len == @as(i32,@intCast(0)));
}

fn test_example_1() void {
    expect((isValid("()") == true));
}

fn test_example_2() void {
    expect((isValid("()[]{}") == true));
}

fn test_example_3() void {
    expect((isValid("(]") == false));
}

fn test_example_4() void {
    expect((isValid("([)]") == false));
}

fn test_example_5() void {
    expect((isValid("{[]}") == true));
}

fn test_empty_string() void {
    expect((isValid("") == true));
}

fn test_single_closing() void {
    expect((isValid("]") == false));
}

fn test_unmatched_open() void {
    expect((isValid("((") == false));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_example_4();
    test_example_5();
    test_empty_string();
    test_single_closing();
    test_unmatched_open();
}
