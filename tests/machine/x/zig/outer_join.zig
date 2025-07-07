const std = @import("std");

var customers: []const std.AutoHashMap([]const u8, i32) = undefined;
var orders: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, std.AutoHashMap([]const u8, i32)) = undefined;

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
    }, blk2: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(3))) catch unreachable;
        m.put("name", "Charlie") catch unreachable;
        break :blk2 m;
    }, blk3: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(4))) catch unreachable;
        m.put("name", "Diana") catch unreachable;
        break :blk3 m;
    } };
    orders = &[_]std.AutoHashMap([]const u8, i32){ blk4: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(100))) catch unreachable;
        m.put("customerId", @as(i32, @intCast(1))) catch unreachable;
        m.put("total", @as(i32, @intCast(250))) catch unreachable;
        break :blk4 m;
    }, blk5: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(101))) catch unreachable;
        m.put("customerId", @as(i32, @intCast(2))) catch unreachable;
        m.put("total", @as(i32, @intCast(125))) catch unreachable;
        break :blk5 m;
    }, blk6: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(102))) catch unreachable;
        m.put("customerId", @as(i32, @intCast(1))) catch unreachable;
        m.put("total", @as(i32, @intCast(300))) catch unreachable;
        break :blk6 m;
    }, blk7: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(103))) catch unreachable;
        m.put("customerId", @as(i32, @intCast(5))) catch unreachable;
        m.put("total", @as(i32, @intCast(80))) catch unreachable;
        break :blk7 m;
    } };
    result = blk9: {
        var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, std.AutoHashMap([]const u8, i32))).init(std.heap.page_allocator);
        for (orders) |o| {
            for (customers) |c| {
                if (!((o.customerId == c.id))) continue;
                _tmp0.append(blk8: {
                    var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                    m.put("order", o) catch unreachable;
                    m.put("customer", c) catch unreachable;
                    break :blk8 m;
                }) catch unreachable;
            }
        }
        const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
        break :blk9 _tmp1;
    };
    std.debug.print("{s}\n", .{"--- Outer Join using syntax ---"});
    for (result) |row| {
        if (row.order) {
            if (row.customer) {
                std.debug.print("{s} {any} {s} {any} {s} {any}\n", .{ "Order", row.order.id, "by", row.customer.name, "- $", row.order.total });
            } else {
                std.debug.print("{s} {any} {s} {s} {s} {any}\n", .{ "Order", row.order.id, "by", "Unknown", "- $", row.order.total });
            }
        } else {
            std.debug.print("{s} {any} {s}\n", .{ "Customer", row.customer.name, "has no orders" });
        }
    }
}
