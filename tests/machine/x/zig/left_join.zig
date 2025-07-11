const std = @import("std");

const CustomersItem = struct {
    id: i32,
    name: []const u8,
};
const customers = &[_]CustomersItem{
    CustomersItem{
    .id = 1,
    .name = "Alice",
},
    CustomersItem{
    .id = 2,
    .name = "Bob",
},
};
const OrdersItem = struct {
    id: i32,
    customerId: i32,
    total: i32,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .id = 100,
    .customerId = 1,
    .total = 250,
},
    OrdersItem{
    .id = 101,
    .customerId = 3,
    .total = 80,
},
};
const result = blk0: { var _tmp0 = std.ArrayList(struct {
    orderId: i32,
    customer: CustomersItem,
    total: i32,
}).init(std.heap.page_allocator); for (orders) |o| { var matched = false; for (customers) |c| { if (!((o.customerId == c.id))) continue; matched = true; _tmp0.append(struct {
    orderId: i32,
    customer: CustomersItem,
    total: i32,
}{
    .orderId = o.id,
    .customer = c,
    .total = o.total,
}) catch unreachable; } if (!matched) { const c: ?CustomersItem = null; _tmp0.append(struct {
    orderId: i32,
    customer: CustomersItem,
    total: i32,
}{
    .orderId = o.id,
    .customer = c,
    .total = o.total,
}) catch unreachable; } } const res = _tmp0.toOwnedSlice() catch unreachable; break :blk0 res; };

pub fn main() void {
    std.debug.print("--- Left Join ---\n", .{});
    for (result) |entry| {
        std.debug.print("Order {any} customer {any} total {any}\n", .{entry.orderId, entry.customer, entry.total});
    }
}
