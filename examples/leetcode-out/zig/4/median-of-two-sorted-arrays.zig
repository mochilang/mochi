const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn findMedianSortedArrays(nums1: []const i32, nums2: []const i32) f64 {
    var merged = std.ArrayList(i32).init(std.heap.page_allocator);
    var i: i32 = @as(i32,@intCast(0));
    var j: i32 = @as(i32,@intCast(0));
    while ((((i < (nums1).len) or j) < (nums2).len)) {
        if ((j >= (nums2).len)) {
            try merged.append(@as(i32,@intCast(nums1[i])));
            i = (i + @as(i32,@intCast(1)));
        } else         if ((i >= (nums1).len)) {
            try merged.append(@as(i32,@intCast(nums2[j])));
            j = (j + @as(i32,@intCast(1)));
        } else         if ((nums1[i] <= nums2[j])) {
            try merged.append(@as(i32,@intCast(nums1[i])));
            i = (i + @as(i32,@intCast(1)));
        } else {
            try merged.append(@as(i32,@intCast(nums2[j])));
            j = (j + @as(i32,@intCast(1)));
        }
    }
    const total: i32 = (merged).len;
    if ((@mod(total, @as(i32,@intCast(2))) == @as(i32,@intCast(1)))) {
        return @as(f64, merged.items[(total / @as(i32,@intCast(2)))]);
    }
    const mid1: i32 = merged[((total / @as(i32,@intCast(2))) - @as(i32,@intCast(1)))];
    const mid2: i32 = merged[(total / @as(i32,@intCast(2)))];
    return (@as(f64, ((mid1 + mid2))) / 2);
}

fn test_example_1() void {
    expect((findMedianSortedArrays(&[_]i32{@as(i32,@intCast(1)), @as(i32,@intCast(3))}, &[_]i32{@as(i32,@intCast(2))}) == 2));
}

fn test_example_2() void {
    expect((findMedianSortedArrays(&[_]i32{@as(i32,@intCast(1)), @as(i32,@intCast(2))}, &[_]i32{@as(i32,@intCast(3)), @as(i32,@intCast(4))}) == 2.5));
}

fn test_empty_first() void {
    expect((findMedianSortedArrays(@as([]const i32, &[_]i32{}), &[_]i32{@as(i32,@intCast(1))}) == 1));
}

fn test_empty_second() void {
    expect((findMedianSortedArrays(&[_]i32{@as(i32,@intCast(2))}, @as([]const i32, &[_]i32{})) == 2));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_empty_first();
    test_empty_second();
}
