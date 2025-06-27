const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn lengthOfLongestSubstring(s: []const u8) i32 {
    const n: i32 = (s).len;
    var start: i32 = @as(i32, @intCast(0));
    var best: i32 = @as(i32, @intCast(0));
    var i: i32 = @as(i32, @intCast(0));
    while ((i < n)) {
        var j: i32 = start;
        while ((j < i)) {
            if ((s[j] == s[i])) {
                start = (j + @as(i32, @intCast(1)));
                break;
            }
            j = (j + @as(i32, @intCast(1)));
        }
        const length: i32 = ((i - start) + @as(i32, @intCast(1)));
        if ((length > best)) {
            best = length;
        }
        i = (i + @as(i32, @intCast(1)));
    }
    return best;
}

fn test_example_1() void {
    expect((lengthOfLongestSubstring("abcabcbb") == @as(i32, @intCast(3))));
}

fn test_example_2() void {
    expect((lengthOfLongestSubstring("bbbbb") == @as(i32, @intCast(1))));
}

fn test_example_3() void {
    expect((lengthOfLongestSubstring("pwwkew") == @as(i32, @intCast(3))));
}

fn test_empty_string() void {
    expect((lengthOfLongestSubstring("") == @as(i32, @intCast(0))));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_empty_string();
}
