const std = @import("std");

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
    CustomersItem{
    .id = 3,
    .name = "Charlie",
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
const stats = blk1: { var _tmp2 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(CustomersItem) }).init(std.heap.page_allocator); var _tmp3 = std.StringHashMap(usize).init(std.heap.page_allocator); for (customers) |c| { for (orders) |o| { if (!((o.customerId == c.id))) continue; const _tmp4 = c.name; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(CustomersItem) }{ .key = _tmp4, .Items = std.ArrayList(CustomersItem).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } var _tmp5 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(CustomersItem) }).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(g) catch unreachable; } var _tmp6 = std.ArrayList(struct {
    name: i32,
    count: i32,
}).init(std.heap.page_allocator);for (_tmp5.items) |g| { _tmp6.append(struct {
    name: i32,
    count: i32,
}{
    .name = g.key,
    .count = (blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |r| { if (!(r.o)) continue; _tmp0.append(r) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; }).len,
}) catch unreachable; } const _tmp6Slice = _tmp6.toOwnedSlice() catch unreachable; break :blk1 _tmp6Slice; }; // []const i32

pub fn main() void {
    std.debug.print("--- Group Left Join ---\n", .{});
    for (stats) |s| {
        std.debug.print("{any} {s} {any}\n", .{s.name, "orders:", s.count});
    }
}
