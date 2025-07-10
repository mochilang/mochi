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
    .customerId = 3,
    .total = 80,
},
}; break :blk1 _arr; });
const result = blk2: { var _tmp2 = std.ArrayList(struct {
    orderId: i32,
    customer: struct {
    id: i32,
    name: []const u8,
},
    total: i32,
}).init(std.heap.page_allocator); for (orders) |o| { var matched = false; for (customers) |c| { if (!((o.customerId == c.id))) continue; matched = true; _tmp2.append(struct {
    orderId: i32,
    customer: struct {
    id: i32,
    name: []const u8,
},
    total: i32,
}{
    .orderId = o.id,
    .customer = c,
    .total = o.total,
}) catch unreachable; } if (!matched) { const c: ?struct {
    id: i32,
    name: []const u8,
} = null; _tmp2.append(struct {
    orderId: i32,
    customer: struct {
    id: i32,
    name: []const u8,
},
    total: i32,
}{
    .orderId = o.id,
    .customer = c,
    .total = o.total,
}) catch unreachable; } } const res = _tmp2.toOwnedSlice() catch unreachable; break :blk2 res; };

pub fn main() void {
    std.debug.print("--- Left Join ---\n", .{});
    for (result) |entry| {
        std.debug.print("Order {any} customer {any} total {any}\n", .{entry.orderId, entry.customer, entry.total});
    }
}
