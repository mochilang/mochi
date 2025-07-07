const std = @import("std");

var customers: []const std.AutoHashMap([]const u8, i32) = undefined;
var orders: []const std.AutoHashMap([]const u8, i32) = undefined;
var items: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

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
        m.put("customerId", @as(i32, @intCast(2))) catch unreachable;
        break :blk3 m;
    } };
    items = &[_]std.AutoHashMap([]const u8, i32){blk4: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("orderId", @as(i32, @intCast(100))) catch unreachable;
        m.put("sku", "a") catch unreachable;
        break :blk4 m;
    }};
    result = blk6: {
        var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (orders) |o| {
            for (customers) |c| {
                if (!((o.customerId == c.id))) continue;
                for (items) |i| {
                    if (!((o.id == i.orderId))) continue;
                    _tmp0.append(blk5: {
                        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                        m.put("orderId", o.id) catch unreachable;
                        m.put("name", c.name) catch unreachable;
                        m.put("item", i) catch unreachable;
                        break :blk5 m;
                    }) catch unreachable;
                }
            }
        }
        const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
        break :blk6 _tmp1;
    };
    std.debug.print("{s}\n", .{"--- Left Join Multi ---"});
    for (result) |r| {
        std.debug.print("{any} {any} {any}\n", .{ r.orderId, r.name, r.item });
    }
}
