const std = @import("std");

fn maxArea(height: []const i32) i32 {
	var left = @as(i32,@intCast(0));
	var right = ((height).len - @as(i32,@intCast(1)));
	var maxArea = @as(i32,@intCast(0));
	while ((left < right)) {
		const width = (right - left);
		var h = @as(i32,@intCast(0));
		if ((height[left] < height[right])) {
			h = height[left];
		} else {
			h = height[right];
		}
		const area = (h * width);
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

pub fn main() void {
}
