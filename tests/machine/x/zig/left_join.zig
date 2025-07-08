const std = @import("std");

var customers: []const std.AutoHashMap([]const u8, i32) = undefined;
var orders: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    customers = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 1) catch unreachable; m.put("name", "Alice") catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 2) catch unreachable; m.put("name", "Bob") catch unreachable; break :blk1 m; }};
    orders = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 100) catch unreachable; m.put("customerId", 1) catch unreachable; m.put("total", 250) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 101) catch unreachable; m.put("customerId", 3) catch unreachable; m.put("total", 80) catch unreachable; break :blk3 m; }};
    result = blk5: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { if (!((o.customerId == c.id))) continue; _tmp0.append(blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("orderId", o.id) catch unreachable; m.put("customer", c) catch unreachable; m.put("total", o.total) catch unreachable; break :blk4 m; }) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk5 _tmp1; };
    std.debug.print("{s}\n", .{"--- Left Join ---"});
    for (result) |entry| {
        std.debug.print("{s} {any} {s} {any} {s} {any}\n", .{"Order", entry.orderId, "customer", entry.customer, "total", entry.total});
    }
}
