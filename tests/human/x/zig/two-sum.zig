const std = @import("std");

fn twoSum(nums: []const i32, target: i32) [2]i32 {
    const n = nums.len;
    var i: usize = 0;
    while (i < n) : (i += 1) {
        var j: usize = i + 1;
        while (j < n) : (j += 1) {
            if (nums[i] + nums[j] == target) {
                return [2]i32{ @intCast(i32, i), @intCast(i32, j) };
            }
        }
    }
    return [2]i32{-1, -1};
}

pub fn main() void {
    const arr = [_]i32{2,7,11,15};
    const result = twoSum(&arr, 9);
    std.debug.print("{d}\n", .{result[0]});
    std.debug.print("{d}\n", .{result[1]});
}
