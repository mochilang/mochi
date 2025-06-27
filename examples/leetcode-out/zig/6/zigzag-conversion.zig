const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _concat_string(a: []const u8, b: []const u8) []const u8 {
    var res = std.ArrayList(u8).init(std.heap.page_allocator);
    defer res.deinit();
    res.appendSlice(a) catch unreachable;
    res.appendSlice(b) catch unreachable;
    return res.toOwnedSlice() catch unreachable;
}

fn convert(s: []const u8, numRows: i32) []const u8 {
    if ((((numRows <= @as(i32,@intCast(1))) or numRows) >= (s).len)) {
        return s;
    }
    var rows = std.ArrayList(u8).init(std.heap.page_allocator);
    var i: i32 = @as(i32,@intCast(0));
    while ((i < numRows)) {
        try rows.append(@as(i32,@intCast("")));
        i = (i + @as(i32,@intCast(1)));
    }
    var curr: i32 = @as(i32,@intCast(0));
    var step: i32 = @as(i32,@intCast(1));
    for (s) |ch| {
        rows.items[curr] = (rows[curr] + ch);
        if ((curr == @as(i32,@intCast(0)))) {
            step = @as(i32,@intCast(1));
        } else         if (((curr == numRows) - @as(i32,@intCast(1)))) {
            step = -@as(i32,@intCast(1));
        }
        curr = (curr + step);
    }
    var result: []const u8 = "";
    for (rows) |row| {
        result = _concat_string(result, row);
    }
    return result;
}

fn test_example_1() void {
    expect(std.mem.eql(u8, convert("PAYPALISHIRING", @as(i32,@intCast(3))), "PAHNAPLSIIGYIR"));
}

fn test_example_2() void {
    expect(std.mem.eql(u8, convert("PAYPALISHIRING", @as(i32,@intCast(4))), "PINALSIGYAHRPI"));
}

fn test_single_row() void {
    expect(std.mem.eql(u8, convert("A", @as(i32,@intCast(1))), "A"));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_single_row();
}
