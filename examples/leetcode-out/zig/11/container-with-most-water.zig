const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn maxArea(height: []const i32) i32 {
    var left: i32 = @as(i32,@intCast(0));
    var right: i32 = ((height).len - @as(i32,@intCast(1)));
    var maxArea: i32 = @as(i32,@intCast(0));
    while ((left < right)) {
        const width: i32 = (right - left);
        var h: i32 = @as(i32,@intCast(0));
        if ((height[left] < height[right])) {
            h = height[left];
        } else {
            h = height[right];
        }
        const area: i32 = (h * width);
        if ((area > maxArea)) {
            maxArea = area;
        }
        if ((height[left] < height[right])) {
            left = (left + @as(i32,@intCast(1)));
        } else {
            right = (right - @as(i32,@intCast(1)));
        }
    }
    return maxArea;
}

fn test_example_1() void {
    expect((maxArea(&[_]i32{@as(i32,@intCast(1)), @as(i32,@intCast(8)), @as(i32,@intCast(6)), @as(i32,@intCast(2)), @as(i32,@intCast(5)), @as(i32,@intCast(4)), @as(i32,@intCast(8)), @as(i32,@intCast(3)), @as(i32,@intCast(7))}) == @as(i32,@intCast(49))));
}

fn test_example_2() void {
    expect((maxArea(&[_]i32{@as(i32,@intCast(1)), @as(i32,@intCast(1))}) == @as(i32,@intCast(1))));
}

fn test_decreasing_heights() void {
    expect((maxArea(&[_]i32{@as(i32,@intCast(4)), @as(i32,@intCast(3)), @as(i32,@intCast(2)), @as(i32,@intCast(1)), @as(i32,@intCast(4))}) == @as(i32,@intCast(16))));
}

fn test_short_array() void {
    expect((maxArea(&[_]i32{@as(i32,@intCast(1)), @as(i32,@intCast(2)), @as(i32,@intCast(1))}) == @as(i32,@intCast(2))));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_decreasing_heights();
    test_short_array();
}
