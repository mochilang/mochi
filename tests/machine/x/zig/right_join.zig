const std = @import("std");

const customers = (blk0: { const _tmp0 = struct {
    id: i32,
    name: []const u8,
}; const _arr = &[_]_tmp0{
    _tmp0{
    .id = 1,
    .name = "Alice",
},
    _tmp0{
    .id = 2,
    .name = "Bob",
},
    _tmp0{
    .id = 3,
    .name = "Charlie",
},
    _tmp0{
    .id = 4,
    .name = "Diana",
},
}; break :blk0 _arr; });
const orders = (blk1: { const _tmp1 = struct {
    id: i32,
    customerId: i32,
    total: i32,
}; const _arr = &[_]_tmp1{
    _tmp1{
    .id = 100,
    .customerId = 1,
    .total = 250,
},
    _tmp1{
    .id = 101,
    .customerId = 2,
    .total = 125,
},
    _tmp1{
    .id = 102,
    .customerId = 1,
    .total = 300,
},
}; break :blk1 _arr; });
const result = blk2: { var _tmp2 = std.ArrayList(struct {
    customerName: []const u8,
    order: struct {
    id: i32,
    customerId: i32,
    total: i32,
},
}).init(std.heap.page_allocator); for (orders) |o| { var matched = false; for (customers) |c| { if (!((o.customerId == c.id))) continue; matched = true; _tmp2.append(struct {
    customerName: []const u8,
    order: struct {
    id: i32,
    customerId: i32,
    total: i32,
},
}{
    .customerName = c.name,
    .order = o,
}) catch unreachable; } if (!matched) { const c: ?struct {
    id: i32,
    name: []const u8,
} = null; _tmp2.append(struct {
    customerName: []const u8,
    order: struct {
    id: i32,
    customerId: i32,
    total: i32,
},
}{
    .customerName = c.name,
    .order = o,
}) catch unreachable; } } const res = _tmp2.toOwnedSlice() catch unreachable; break :blk2 res; };

pub fn main() void {
    std.debug.print("--- Right Join using syntax ---\n", .{});
    for (result) |entry| {
        if (entry.order) {
            std.debug.print("Customer {any} has order {any} - $ {any}\n", .{entry.customerName, entry.order.id, entry.order.total});
        } else {
            std.debug.print("Customer {any} has no orders\n", .{entry.customerName});
        }
    }
}
