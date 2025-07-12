const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
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
    .customerId = 1,
},
    OrdersItem{
    .id = 102,
    .customerId = 2,
},
}; // []const OrdersItem
const ResultStruct0 = struct {
    name: i32,
    count: i32,
};
const stats = blk0: { var _tmp1 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(OrdersItem) }).init(std.heap.page_allocator); var _tmp2 = std.StringHashMap(usize).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { if (!((o.customerId == c.id))) continue; const _tmp3 = c.name; if (_tmp2.get(_tmp3)) |idx| { _tmp1.items[idx].Items.append(o) catch |err| handleError(err); } else { var g = struct { key: []const u8, Items: std.ArrayList(OrdersItem) }{ .key = _tmp3, .Items = std.ArrayList(OrdersItem).init(std.heap.page_allocator) }; g.Items.append(o) catch |err| handleError(err); _tmp1.append(g) catch |err| handleError(err); _tmp2.put(_tmp3, _tmp1.items.len - 1) catch |err| handleError(err); } } } var _tmp4 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(OrdersItem) }).init(std.heap.page_allocator);for (_tmp1.items) |g| { _tmp4.append(g) catch |err| handleError(err); } var _tmp5 = std.ArrayList(struct {
    name: i32,
    count: i32,
}).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp5.append(ResultStruct0{
    .name = g.key,
    .count = (g.Items.len),
}) catch |err| handleError(err); } const _tmp5Slice = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp5Slice; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- Orders per customer ---\n", .{});
    for (stats) |s| {
        std.debug.print("{any} {s} {any}\n", .{s.name, "orders:", s.count});
    }
}
