const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
    return false;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch |err| handleError(err);
    std.debug.print("{s}\n", .{buf.items});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return std.meta.eql(a, b);
}

const CustomerItem = struct { c_custkey: i32, };
const customer = &[_]CustomerItem{
    CustomerItem{ .c_custkey = 1 },
    CustomerItem{ .c_custkey = 2 },
    CustomerItem{ .c_custkey = 3 },
}; // []const CustomerItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
    o_comment: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 100,
    .o_custkey = 1,
    .o_comment = "fast delivery",
},
    OrdersItem{
    .o_orderkey = 101,
    .o_custkey = 1,
    .o_comment = "no comment",
},
    OrdersItem{
    .o_orderkey = 102,
    .o_custkey = 2,
    .o_comment = "special requests only",
},
}; // []const OrdersItem
const PerCustomerItem = struct { c_count: i32, };
var per_customer: []const PerCustomerItem = undefined; // []const PerCustomerItem
const GroupedItem = struct {
    c_count: i32,
    custdist: i32,
};
const ResultStruct6 = struct { key: i32, Items: std.ArrayList(PerCustomerItem) };
var grouped: []const GroupedItem = undefined; // []const GroupedItem

fn test_Q13_groups_customers_by_non_special_order_count() void {
    expect((grouped == &[_]GroupedItem{
    GroupedItem{
    .c_count = 2,
    .custdist = 1,
},
    GroupedItem{
    .c_count = 0,
    .custdist = 2,
},
}));
}

pub fn main() void {
    per_customer = blk1: { var _tmp3 = std.ArrayList(PerCustomerItem).init(std.heap.page_allocator); for (customer) |c| { _tmp3.append(PerCustomerItem{ .c_count = (blk0: { var _tmp1 = std.ArrayList(OrdersItem).init(std.heap.page_allocator); for (orders) |o| { if (!(((((o.o_custkey == c.c_custkey) and (!(_contains_list_int(o.o_comment, "special")))) and (!(_contains_list_int(o.o_comment, "requests"))))))) continue; _tmp1.append(o) catch |err| handleError(err); } const _tmp2 = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp2; }).len }) catch |err| handleError(err); } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; };
    grouped = blk2: { var _tmp7 = std.ArrayList(ResultStruct6).init(std.heap.page_allocator); for (per_customer) |x| { const _tmp8 = x.c_count; var _found = false; var _idx: usize = 0; for (_tmp7.items, 0..) |it, i| { if (_equal(it.key, _tmp8)) { _found = true; _idx = i; break; } } if (_found) { _tmp7.items[_idx].Items.append(x) catch |err| handleError(err); } else { var g = ResultStruct6{ .key = _tmp8, .Items = std.ArrayList(PerCustomerItem).init(std.heap.page_allocator) }; g.Items.append(x) catch |err| handleError(err); _tmp7.append(g) catch |err| handleError(err); } } var _tmp9 = std.ArrayList(ResultStruct6).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp9.append(g) catch |err| handleError(err); } var _tmp10 = std.ArrayList(struct { item: ResultStruct6, key: i32 }).init(std.heap.page_allocator);for (_tmp9.items) |g| { _tmp10.append(.{ .item = g, .key = -g.key }) catch |err| handleError(err); } for (0.._tmp10.items.len) |i| { for (i+1.._tmp10.items.len) |j| { if (_tmp10.items[j].key < _tmp10.items[i].key) { const t = _tmp10.items[i]; _tmp10.items[i] = _tmp10.items[j]; _tmp10.items[j] = t; } } } var _tmp11 = std.ArrayList(ResultStruct6).init(std.heap.page_allocator);for (_tmp10.items) |p| { _tmp11.append(p.item) catch |err| handleError(err); } var _tmp12 = std.ArrayList(struct {
    c_count: i32,
    custdist: i32,
}).init(std.heap.page_allocator);for (_tmp11.items) |g| { _tmp12.append(GroupedItem{
    .c_count = g.key,
    .custdist = (g.Items.items.len),
}) catch |err| handleError(err); } const _tmp12Slice = _tmp12.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp12Slice; };
    _json(grouped);
    test_Q13_groups_customers_by_non_special_order_count();
}
