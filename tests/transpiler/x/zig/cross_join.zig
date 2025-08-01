// Generated by Mochi Zig transpiler on 2025-07-21 10:57 +0000
const std = @import("std");

const Customer = struct {
    id: i64,
    name: []const u8,
};

const Entry = struct {
    order_id: i64,
    order_customer_id: i64,
    paired_customer_name: []const u8,
    order_total: i64,
};

pub fn main() void {
    const customers: [_]Customer = [_]Customer{.{ .id = 1, .name = "Alice" }, .{ .id = 2, .name = "Bob" }, .{ .id = 3, .name = "Charlie" }};
    const orders = [_]std.StringHashMap(i64){blk: { var m = std.StringHashMap(i64).init(std.heap.page_allocator); m.put("id", 100) catch unreachable; m.put("customerId", 1) catch unreachable; m.put("total", 250) catch unreachable; break :blk m; }, blk: { var m = std.StringHashMap(i64).init(std.heap.page_allocator); m.put("id", 101) catch unreachable; m.put("customerId", 2) catch unreachable; m.put("total", 125) catch unreachable; break :blk m; }, blk: { var m = std.StringHashMap(i64).init(std.heap.page_allocator); m.put("id", 102) catch unreachable; m.put("customerId", 1) catch unreachable; m.put("total", 300) catch unreachable; break :blk m; }};
    const result: []Entry = blk: {
    var arr = std.ArrayList(Entry).init(std.heap.page_allocator);
    for (orders) |o| {
        for (customers) |c| {
            arr.append(.{ .order_id = o.id, .order_customer_id = o.customer_id, .paired_customer_name = c.name, .order_total = o.total }) catch unreachable;
        }
    }
    var tmp = arr.toOwnedSlice() catch unreachable;
    break :blk tmp;
};
    try std.io.getStdOut().writer().print("{s}\n", .{"--- Cross Join: All order-customer pairs ---"});
    for (result) |entry| {
        try std.io.getStdOut().writer().print("{s} {any} {s} {any} {s} {any} {s} {any}\n", .{"Order", entry.order_id, "(customerId:", entry.order_customer_id, ", total: $", entry.order_total, ") paired with", entry.paired_customer_name});
    }
}
