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
    CustomersItem{
    .id = 3,
    .name = "Charlie",
},
}; // []const CustomersItem
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
    .customerId = 2,
    .total = 125,
},
    OrdersItem{
    .id = 102,
    .customerId = 1,
    .total = 300,
},
}; // []const OrdersItem
const result = blk0: { var _tmp0 = std.ArrayList(struct {
    orderId: i32,
    orderCustomerId: i32,
    pairedCustomerName: []const u8,
    orderTotal: i32,
}).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { _tmp0.append(struct {
    orderId: i32,
    orderCustomerId: i32,
    pairedCustomerName: []const u8,
    orderTotal: i32,
}{
    .orderId = o.id,
    .orderCustomerId = o.customerId,
    .pairedCustomerName = c.name,
    .orderTotal = o.total,
}) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- Cross Join: All order-customer pairs ---\n", .{});
    for (result) |entry| {
        std.debug.print("Order {any} (customerId: {any} , total: $ {any} ) paired with {any}\n", .{entry.orderId, entry.orderCustomerId, entry.orderTotal, entry.pairedCustomerName});
    }
}
