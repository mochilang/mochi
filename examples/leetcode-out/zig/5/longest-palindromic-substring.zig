const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn expand(s: []const u8, left: i32, right: i32) i32 {
    var l: i32 = left;
    var r: i32 = right;
    const n: i32 = (s).len;
    while ((((l >= @as(i32, @intCast(0))) and r) < n)) {
        if ((s[l] != s[r])) {
            break;
        }
        l = (l - @as(i32, @intCast(1)));
        r = (r + @as(i32, @intCast(1)));
    }
    return ((r - l) - @as(i32, @intCast(1)));
}

pub fn longestPalindrome(s: []const u8) []const u8 {
    if (((s).len <= @as(i32, @intCast(1)))) {
        return s;
    }
    var start: i32 = @as(i32, @intCast(0));
    var end: i32 = @as(i32, @intCast(0));
    const n: i32 = (s).len;
    for (@as(i32, @intCast(0))..n) |i| {
        const len1: i32 = expand(s, i, i);
        const len2: i32 = expand(s, i, (i + @as(i32, @intCast(1))));
        var l: i32 = len1;
        if ((len2 > len1)) {
            l = len2;
        }
        if ((l > ((end - start)))) {
            start = (i - ((((l - @as(i32, @intCast(1)))) / @as(i32, @intCast(2)))));
            end = (i + ((l / @as(i32, @intCast(2)))));
        }
    }
    return s[start..(end + @as(i32, @intCast(1)))];
}

fn test_example_1() void {
    const ans: []const u8 = longestPalindrome("babad");
    expect(std.mem.eql(u8, (std.mem.eql(u8, ans, "bab") or ans), "aba"));
}

fn test_example_2() void {
    expect(std.mem.eql(u8, longestPalindrome("cbbd"), "bb"));
}

fn test_single_char() void {
    expect(std.mem.eql(u8, longestPalindrome("a"), "a"));
}

fn test_two_chars() void {
    const ans: []const u8 = longestPalindrome("ac");
    expect(std.mem.eql(u8, (std.mem.eql(u8, ans, "a") or ans), "c"));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_single_char();
    test_two_chars();
}
