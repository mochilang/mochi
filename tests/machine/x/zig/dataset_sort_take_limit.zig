const std = @import("std");

fn _slice_list(comptime T: type, v: []const T, start: i32, end: i32, step: i32) []T {
    var s = start;
    var e = end;
    var st = step;
    const n: i32 = @as(i32, @intCast(v.len));
    if (s < 0) s += n;
    if (e < 0) e += n;
    if (st == 0) st = 1;
    if (s < 0) s = 0;
    if (e > n) e = n;
    if (st > 0 and e < s) e = s;
    if (st < 0 and e > s) e = s;
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    var i: i32 = s;
    while ((st > 0 and i < e) or (st < 0 and i > e)) : (i += st) {
        res.append(v[@as(usize, @intCast(i))]) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

var products: []const std.AutoHashMap([]const u8, i32) = undefined;
var expensive: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    products = &[_]std.AutoHashMap([]const u8, i32){ blk0: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Laptop") catch unreachable;
        m.put("price", @as(i32, @intCast(1500))) catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Smartphone") catch unreachable;
        m.put("price", @as(i32, @intCast(900))) catch unreachable;
        break :blk1 m;
    }, blk2: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Tablet") catch unreachable;
        m.put("price", @as(i32, @intCast(600))) catch unreachable;
        break :blk2 m;
    }, blk3: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Monitor") catch unreachable;
        m.put("price", @as(i32, @intCast(300))) catch unreachable;
        break :blk3 m;
    }, blk4: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Keyboard") catch unreachable;
        m.put("price", @as(i32, @intCast(100))) catch unreachable;
        break :blk4 m;
    }, blk5: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Mouse") catch unreachable;
        m.put("price", @as(i32, @intCast(50))) catch unreachable;
        break :blk5 m;
    }, blk6: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Headphones") catch unreachable;
        m.put("price", @as(i32, @intCast(200))) catch unreachable;
        break :blk6 m;
    } };
    expensive = blk7: {
        var _tmp0 = std.ArrayList(struct { item: std.AutoHashMap([]const u8, i32), key: i32 }).init(std.heap.page_allocator);
        for (products) |p| {
            _tmp0.append(.{ .item = p, .key = -p.price }) catch unreachable;
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
        var _tmp1 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (_tmp0.items) |p| {
            _tmp1.append(p.item) catch unreachable;
        }
        const _tmp2 = _tmp1.toOwnedSlice() catch unreachable;
        _tmp2 = _slice_list(std.AutoHashMap([]const u8, i32), _tmp2, @as(i32, @intCast(1)), (@as(i32, @intCast(1)) + @as(i32, @intCast(3))), 1);
        break :blk7 _tmp2;
    };
    std.debug.print("{s}\n", .{"--- Top products (excluding most expensive) ---"});
    for (expensive) |item| {
        std.debug.print("{any} {s} {any}\n", .{ item.name, "costs $", item.price });
    }
}
