const std = @import("std");

var items: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const i32 = undefined;

pub fn main() void {
    items = &[_]std.AutoHashMap([]const u8, i32){ blk0: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("n", @as(i32, @intCast(1))) catch unreachable;
        m.put("v", "a") catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("n", @as(i32, @intCast(1))) catch unreachable;
        m.put("v", "b") catch unreachable;
        break :blk1 m;
    }, blk2: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("n", @as(i32, @intCast(2))) catch unreachable;
        m.put("v", "c") catch unreachable;
        break :blk2 m;
    } };
    result = blk3: {
        var _tmp0 = std.ArrayList(struct { item: i32, key: i32 }).init(std.heap.page_allocator);
        for (items) |i| {
            _tmp0.append(.{ .item = i.v, .key = i.n }) catch unreachable;
        }
        for (0.._tmp0.items.len) |i| {
            for (i + 1.._tmp0.items.len) |j| {
                if (_tmp0.items[j].key < _tmp0.items[i].key) {
                    const t = _tmp0.items[i];
                    _tmp0.items[i] = _tmp0.items[j];
                    _tmp0.items[j] = t;
                }
            }
        }
        var _tmp1 = std.ArrayList(i32).init(std.heap.page_allocator);
        for (_tmp0.items) |p| {
            _tmp1.append(p.item) catch unreachable;
        }
        const _tmp2 = _tmp1.toOwnedSlice() catch unreachable;
        break :blk3 _tmp2;
    };
    std.debug.print("{any}\n", .{result});
}
