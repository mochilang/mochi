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

fn intToRoman(num: i32) []const u8 {
    const values: []const i32 = &[_]i32{ @as(i32, @intCast(1000)), @as(i32, @intCast(900)), @as(i32, @intCast(500)), @as(i32, @intCast(400)), @as(i32, @intCast(100)), @as(i32, @intCast(90)), @as(i32, @intCast(50)), @as(i32, @intCast(40)), @as(i32, @intCast(10)), @as(i32, @intCast(9)), @as(i32, @intCast(5)), @as(i32, @intCast(4)), @as(i32, @intCast(1)) };
    const symbols: []const []const u8 = &[_][]const u8{ "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I" };
    var result: []const u8 = "";
    var i: i32 = @as(i32, @intCast(0));
    while ((num > @as(i32, @intCast(0)))) {
        while ((num >= values[i])) {
            result = _concat_string(result, symbols[i]);
            num = (num - values[i]);
        }
        i = (i + @as(i32, @intCast(1)));
    }
    return result;
}

fn test_example_1() void {
    expect(std.mem.eql(u8, intToRoman(@as(i32, @intCast(3))), "III"));
}

fn test_example_2() void {
    expect(std.mem.eql(u8, intToRoman(@as(i32, @intCast(58))), "LVIII"));
}

fn test_example_3() void {
    expect(std.mem.eql(u8, intToRoman(@as(i32, @intCast(1994))), "MCMXCIV"));
}

fn test_small_numbers() void {
    expect(std.mem.eql(u8, intToRoman(@as(i32, @intCast(4))), "IV"));
    expect(std.mem.eql(u8, intToRoman(@as(i32, @intCast(9))), "IX"));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_small_numbers();
}
