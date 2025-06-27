const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn longestCommonPrefix(strs: []const []const u8) []const u8 {
    if (((strs).len == @as(i32, @intCast(0)))) {
        return "";
    }
    var prefix: i32 = strs[@as(i32, @intCast(0))];
    for (@as(i32, @intCast(1))..(strs).len) |i| {
        var j: i32 = @as(i32, @intCast(0));
        const current: i32 = strs[i];
        while ((((j < (prefix).len) and j) < (current).len)) {
            if ((prefix[j] != current[j])) {
                break;
            }
            j = (j + @as(i32, @intCast(1)));
        }
        prefix = prefix[@as(i32, @intCast(0))..j];
        if (std.mem.eql(u8, prefix, "")) {
            break;
        }
    }
    return prefix;
}

fn test_example_1() void {
    expect(std.mem.eql(u8, longestCommonPrefix(&[_][]const u8{ "flower", "flow", "flight" }), "fl"));
}

fn test_example_2() void {
    expect(std.mem.eql(u8, longestCommonPrefix(&[_][]const u8{ "dog", "racecar", "car" }), ""));
}

fn test_single_string() void {
    expect(std.mem.eql(u8, longestCommonPrefix(&[_][]const u8{"single"}), "single"));
}

fn test_no_common_prefix() void {
    expect(std.mem.eql(u8, longestCommonPrefix(&[_][]const u8{ "a", "b", "c" }), ""));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_single_string();
    test_no_common_prefix();
}
