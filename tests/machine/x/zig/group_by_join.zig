const std = @import("std");

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var customers: []const std.AutoHashMap([]const u8, i32) = undefined;
var orders: []const std.AutoHashMap([]const u8, i32) = undefined;
var stats: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    customers = &[_]std.AutoHashMap([]const u8, i32){ blk0: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(1))) catch unreachable;
        m.put("name", "Alice") catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(2))) catch unreachable;
        m.put("name", "Bob") catch unreachable;
        break :blk1 m;
    } };
    orders = &[_]std.AutoHashMap([]const u8, i32){ blk2: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(100))) catch unreachable;
        m.put("customerId", @as(i32, @intCast(1))) catch unreachable;
        break :blk2 m;
    }, blk3: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(101))) catch unreachable;
        m.put("customerId", @as(i32, @intCast(1))) catch unreachable;
        break :blk3 m;
    }, blk4: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(102))) catch unreachable;
        m.put("customerId", @as(i32, @intCast(2))) catch unreachable;
        break :blk4 m;
    } };
    stats = blk6: {
        var _tmp0 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator);
        var _tmp1 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator);
        for (orders) |o| {
            for (customers) |c| {
                if (!((o.customerId == c.id))) continue;
                const _tmp2 = c.name;
                if (_tmp1.get(_tmp2)) |idx| {
                    _tmp0.items[idx].Items.append(o) catch unreachable;
                } else {
                    var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp2, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) };
                    g.Items.append(o) catch unreachable;
                    _tmp0.append(g) catch unreachable;
                    _tmp1.put(_tmp2, _tmp0.items.len - 1) catch unreachable;
                }
            }
        }
        var _tmp3 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (_tmp0.items) |g| {
            _tmp3.append(blk5: {
                var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                m.put("name", g.key) catch unreachable;
                m.put(count, (g.Items.len)) catch unreachable;
                break :blk5 m;
            }) catch unreachable;
        }
        break :blk6 _tmp3.toOwnedSlice() catch unreachable;
    };
    std.debug.print("{s}\n", .{"--- Orders per customer ---"});
    for (stats) |s| {
        std.debug.print("{any} {s} {any}\n", .{ s.name, "orders:", s.count });
    }
}
