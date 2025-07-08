const std = @import("std");

var customers: []const std.AutoHashMap([]const u8, i32) = undefined;
var orders: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    customers = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 1) catch unreachable; m.put("name", "Alice") catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 2) catch unreachable; m.put("name", "Bob") catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 3) catch unreachable; m.put("name", "Charlie") catch unreachable; break :blk2 m; }};
    orders = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 100) catch unreachable; m.put("customerId", 1) catch unreachable; m.put("total", 250) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 101) catch unreachable; m.put("customerId", 2) catch unreachable; m.put("total", 125) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 102) catch unreachable; m.put("customerId", 1) catch unreachable; m.put("total", 300) catch unreachable; break :blk5 m; }};
    result = blk7: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { _tmp0.append(blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("orderId", o.id) catch unreachable; m.put("orderCustomerId", o.customerId) catch unreachable; m.put("pairedCustomerName", c.name) catch unreachable; m.put("orderTotal", o.total) catch unreachable; break :blk6 m; }) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; };
    std.debug.print("{s}\n", .{"--- Cross Join: All order-customer pairs ---"});
    for (result) |entry| {
        std.debug.print("{s} {any} {s} {any} {s} {any} {s} {any}\n", .{"Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName});
    }
}
