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
}; // []const CustomersItem
const OrdersItem = struct {
    id: i32,
    customerId: i32,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .id = 100,
    .customerId = 1,
},
    OrdersItem{
    .id = 101,
    .customerId = 2,
},
}; // []const OrdersItem
const ItemsItem = struct {
    orderId: i32,
    sku: []const u8,
};
const items = &[_]ItemsItem{
    ItemsItem{
    .orderId = 100,
    .sku = "a",
},
    ItemsItem{
    .orderId = 101,
    .sku = "b",
},
}; // []const ItemsItem
const result = blk0: { var _tmp0 = std.ArrayList(struct {
    name: []const u8,
    sku: []const u8,
}).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { if (!((o.customerId == c.id))) continue; for (items) |i| { if (!((o.id == i.orderId))) continue; _tmp0.append(struct {
    name: []const u8,
    sku: []const u8,
}{
    .name = c.name,
    .sku = i.sku,
}) catch |err| handleError(err); } } } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }; // []const std.StringHashMap([]const u8)

pub fn main() void {
    std.debug.print("--- Multi Join ---\n", .{});
    for (result) |r| {
        std.debug.print("{s} {s} {s}\n", .{r.name, "bought item", r.sku});
    }
}
