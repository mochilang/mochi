const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn isPalindrome(x: i32) bool {
    if ((x < @as(i32,@intCast(0)))) {
        return false;
    }
    const s: []const u8 = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{x}) catch unreachable;
    const n: i32 = (s).len;
    for (@as(i32,@intCast(0)) .. (n / @as(i32,@intCast(2)))) |i| {
        if ((s[i] != s[((n - @as(i32,@intCast(1))) - i)])) {
            return false;
        }
    }
    return true;
}

fn test_example_1() void {
    expect((isPalindrome(@as(i32,@intCast(121))) == true));
}

fn test_example_2() void {
    expect((isPalindrome(-@as(i32,@intCast(121))) == false));
}

fn test_example_3() void {
    expect((isPalindrome(@as(i32,@intCast(10))) == false));
}

fn test_zero() void {
    expect((isPalindrome(@as(i32,@intCast(0))) == true));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_zero();
}
