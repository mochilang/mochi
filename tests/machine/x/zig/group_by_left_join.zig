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
const ResultStruct0 = struct {
    name: i32,
    count: i32,
};
const stats = blk2: { var _tmp5 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(CustomersItem) }).init(std.heap.page_allocator); var _tmp6 = std.StringHashMap(usize).init(std.heap.page_allocator); for (customers) |c| { for (orders) |o| { if (!((o.customerId == c.id))) continue; const _tmp7 = c.name; if (_tmp6.get(_tmp7)) |idx| { _tmp5.items[idx].Items.append(c) catch |err| handleError(err); } else { var g = struct { key: []const u8, Items: std.ArrayList(CustomersItem) }{ .key = _tmp7, .Items = std.ArrayList(CustomersItem).init(std.heap.page_allocator) }; g.Items.append(c) catch |err| handleError(err); _tmp5.append(g) catch |err| handleError(err); _tmp6.put(_tmp7, _tmp5.items.len - 1) catch |err| handleError(err); } } } var _tmp8 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(CustomersItem) }).init(std.heap.page_allocator);for (_tmp5.items) |g| { _tmp8.append(g) catch |err| handleError(err); } var _tmp9 = std.ArrayList(struct {
    name: i32,
    count: i32,
}).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp9.append(ResultStruct0{
    .name = g.key,
    .count = (blk1: { var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |r| { if (!(r.o)) continue; _tmp3.append(r) catch |err| handleError(err); } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; }).len,
}) catch |err| handleError(err); } const _tmp9Slice = _tmp9.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp9Slice; }; // []const i32

pub fn main() void {
    std.debug.print("--- Group Left Join ---\n", .{});
    for (stats) |s| {
        std.debug.print("{any} {s} {any}\n", .{s.name, "orders:", s.count});
    }
}
