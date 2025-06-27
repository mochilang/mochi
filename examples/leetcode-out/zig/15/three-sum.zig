const std = @import("std");

fn expect(cond: bool) void {
	if (!cond) @panic("expect failed");
}

fn threeSum(nums: []const i32) []const []const i32 {
	const sorted: []const i32 = blk: { var _tmp0 = std.ArrayList(struct { item: i32; key: i32 }).init(std.heap.page_allocator); for (nums) |x| { _tmp0.append(.{ .item = x, .key = x }) catch unreachable; } for (0.._tmp0.items.len) |i| { for (i+1.._tmp0.items.len) |j| { if (_tmp0.items[j].key < _tmp0.items[i].key) { const t = _tmp0.items[i]; _tmp0.items[i] = _tmp0.items[j]; _tmp0.items[j] = t; } } } var _tmp1 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp0.items) |p| { _tmp1.append(p.item) catch unreachable; } var _tmp2 = _tmp1.toOwnedSlice() catch unreachable; break :blk _tmp2; };
	const n: i32 = (sorted).len;
	var res = std.ArrayList(i32).init(std.heap.page_allocator);
	var i: i32 = @as(i32,@intCast(0));
	while ((i < n)) {
		if ((((i > @as(i32,@intCast(0))) and sorted[i]) == sorted[(i - @as(i32,@intCast(1)))])) {
			i = (i + @as(i32,@intCast(1)));
			continue;
		}
		var left: i32 = (i + @as(i32,@intCast(1)));
		var right: i32 = (n - @as(i32,@intCast(1)));
		while ((left < right)) {
			const sum: i32 = ((sorted[i] + sorted[left]) + sorted[right]);
			if ((sum == @as(i32,@intCast(0)))) {
				try res.append(@as(i32,@intCast(&[_]i32{sorted[i], sorted[left], sorted[right]})));
				left = (left + @as(i32,@intCast(1)));
				while ((((left < right) and sorted[left]) == sorted[(left - @as(i32,@intCast(1)))])) {
					left = (left + @as(i32,@intCast(1)));
				}
				right = (right - @as(i32,@intCast(1)));
				while ((((left < right) and sorted[right]) == sorted[(right + @as(i32,@intCast(1)))])) {
					right = (right - @as(i32,@intCast(1)));
				}
			} else 			if ((sum < @as(i32,@intCast(0)))) {
				left = (left + @as(i32,@intCast(1)));
			} else {
				right = (right - @as(i32,@intCast(1)));
			}
		}
		i = (i + @as(i32,@intCast(1)));
	}
	return res.items;
}

fn test_example_1() void {
	expect((threeSum(&[_]i32{-@as(i32,@intCast(1)), @as(i32,@intCast(0)), @as(i32,@intCast(1)), @as(i32,@intCast(2)), -@as(i32,@intCast(1)), -@as(i32,@intCast(4))}) == &[_][]const i32{&[_]i32{-@as(i32,@intCast(1)), -@as(i32,@intCast(1)), @as(i32,@intCast(2))}, &[_]i32{-@as(i32,@intCast(1)), @as(i32,@intCast(0)), @as(i32,@intCast(1))}}));
}

fn test_example_2() void {
	expect((threeSum(&[_]i32{@as(i32,@intCast(0)), @as(i32,@intCast(1)), @as(i32,@intCast(1))}) == &[_]i32{}));
}

fn test_example_3() void {
	expect((threeSum(&[_]i32{@as(i32,@intCast(0)), @as(i32,@intCast(0)), @as(i32,@intCast(0))}) == &[_][]const i32{&[_]i32{@as(i32,@intCast(0)), @as(i32,@intCast(0)), @as(i32,@intCast(0))}}));
}

pub fn main() void {
	test_example_1();
	test_example_2();
	test_example_3();
}
