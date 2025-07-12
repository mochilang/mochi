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
    CustomersItem{
    .id = 4,
    .name = "Diana",
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
    OrdersItem{
    .id = 103,
    .customerId = 5,
    .total = 80,
},
}; // []const OrdersItem
const result = blk0: { var _tmp0 = std.ArrayList(struct {
    order: OrdersItem,
    customer: CustomersItem,
}).init(std.heap.page_allocator); var _tmp1 = std.AutoHashMap(usize, bool).init(std.heap.page_allocator); for (orders, 0..) |o, _i| { var c: ?CustomersItem = null; var mi: usize = 0; for (customers, 0..) |j, ji| { if (!((o.customerId == c.id))) continue; c = j; mi = ji; _tmp1.put(ji, true) catch {}; break; } _tmp0.append(struct {
    order: OrdersItem,
    customer: CustomersItem,
}{
    .order = o,
    .customer = c,
}) catch unreachable; } for (customers, 0..) |j, ji| { if (!_tmp1.contains(ji)) { const o: ?OrdersItem = null; c = j; _tmp0.append(struct {
    order: OrdersItem,
    customer: CustomersItem,
}{
    .order = o,
    .customer = c,
}) catch unreachable; } } const res = _tmp0.toOwnedSlice() catch unreachable; break :blk0 res; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- Outer Join using syntax ---\n", .{});
    for (result) |row| {
        if (row.order) {
            if (row.customer) {
                std.debug.print("Order {any} by {any} - $ {any}\n", .{row.order.id, row.customer.name, row.order.total});
            } else {
                std.debug.print("Order {any} by Unknown - $ {any}\n", .{row.order.id, row.order.total});
            }
        } else {
            std.debug.print("Customer {any} has no orders\n", .{row.customer.name});
        }
    }
}
