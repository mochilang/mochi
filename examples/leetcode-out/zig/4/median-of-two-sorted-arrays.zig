const std = @import("std");

fn findMedianSortedArrays(nums1: []const i32, nums2: []const i32) [2]i32 {
	var merged = std.ArrayList(i32).init(std.heap.page_allocator);
	var i = 0;
	var j = 0;
	while ((((i < nums1.len) or j) < nums2.len)) {
		if ((j >= nums2.len)) {
			try merged.append(@as(i32,@intCast(nums1[i])));
			i = (i + 1);
		} else 		if ((i >= nums1.len)) {
			try merged.append(@as(i32,@intCast(nums2[j])));
			j = (j + 1);
		} else 		if ((nums1[i] <= nums2[j])) {
			try merged.append(@as(i32,@intCast(nums1[i])));
			i = (i + 1);
		} else {
			try merged.append(@as(i32,@intCast(nums2[j])));
			j = (j + 1);
		}
	}
	const total = merged.len;
	if (((total % 2) == 1)) {
		return @as(f64, merged[(total / 2)]);
	}
	const mid1 = merged[((total / 2) - 1)];
	const mid2 = merged[(total / 2)];
	return (@as(f64, ((mid1 + mid2))) / 2);
}

pub fn main() void {
}
