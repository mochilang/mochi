const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn threeSumClosest(nums: []const i32, target: i32) i32 {
    const sorted: []const i32 = blk: { var _tmp0 = std.ArrayList(struct { item: i32; key: i32 }).init(std.heap.page_allocator); for (nums) |n| { _tmp0.append(.{ .item = n, .key = n }) catch unreachable; } for (0.._tmp0.items.len) |i| { for (i+1.._tmp0.items.len) |j| { if (_tmp0.items[j].key < _tmp0.items[i].key) { const t = _tmp0.items[i]; _tmp0.items[i] = _tmp0.items[j]; _tmp0.items[j] = t; } } } var _tmp1 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp0.items) |p| { _tmp1.append(p.item) catch unreachable; } var _tmp2 = _tmp1.toOwnedSlice() catch unreachable; break :blk _tmp2; };
    const n: i32 = (sorted).len;
    var best: i32 = ((sorted[@as(i32,@intCast(0))] + sorted[@as(i32,@intCast(1))]) + sorted[@as(i32,@intCast(2))]);
    for (@as(i32,@intCast(0)) .. n) |i| {
        var left: i32 = (i + @as(i32,@intCast(1)));
        var right: i32 = (n - @as(i32,@intCast(1)));
        while ((left < right)) {
            const sum: i32 = ((sorted[i] + sorted[left]) + sorted[right]);
            if ((sum == target)) {
                return target;
            }
            var diff: i32 = @as(i32,@intCast(0));
            if ((sum > target)) {
                diff = (sum - target);
            } else {
                diff = (target - sum);
            }
            var bestDiff: i32 = @as(i32,@intCast(0));
            if ((best > target)) {
                bestDiff = (best - target);
            } else {
                bestDiff = (target - best);
            }
            if ((diff < bestDiff)) {
                best = sum;
            }
            if ((sum < target)) {
                left = (left + @as(i32,@intCast(1)));
            } else {
                right = (right - @as(i32,@intCast(1)));
            }
        }
    }
    return best;
}

fn test_example_1() void {
    expect((threeSumClosest(&[_]i32{-@as(i32,@intCast(1)), @as(i32,@intCast(2)), @as(i32,@intCast(1)), -@as(i32,@intCast(4))}, @as(i32,@intCast(1))) == @as(i32,@intCast(2))));
}

fn test_example_2() void {
    expect((threeSumClosest(&[_]i32{@as(i32,@intCast(0)), @as(i32,@intCast(0)), @as(i32,@intCast(0))}, @as(i32,@intCast(1))) == @as(i32,@intCast(0))));
}

fn test_additional() void {
    expect((threeSumClosest(&[_]i32{@as(i32,@intCast(1)), @as(i32,@intCast(1)), @as(i32,@intCast(1)), @as(i32,@intCast(0))}, -@as(i32,@intCast(100))) == @as(i32,@intCast(2))));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_additional();
}
