const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

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
}; // []const Customersitem
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
    .customerId = 4,
    .total = 80,
},
}; // []const Ordersitem
const result = blk0: { var _tmp0 = std.ArrayList(struct {
    orderId: i32,
    customerName: []const u8,
    total: i32,
}).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { if (!((o.customerId == c.id))) continue; _tmp0.append(struct {
    orderId: i32,
    customerName: []const u8,
    total: i32,
}{
    .orderId = o.id,
    .customerName = c.name,
    .total = o.total,
}) catch |err| handleError(err); } } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- Orders with customer info ---\n", .{});
    for (result) |entry| {
        std.debug.print("Order {any} by {any} - $ {any}\n", .{entry.orderId, entry.customerName, entry.total});
    }
}
