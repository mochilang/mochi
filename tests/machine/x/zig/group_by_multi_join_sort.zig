const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const NationItem = struct {
    n_nationkey: i32,
    n_name: []const u8,
};
const nation = &[_]NationItem{NationItem{
    .n_nationkey = 1,
    .n_name = "BRAZIL",
}}; // []const Nationitem
const CustomerItem = struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_nationkey: i32,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
};
const customer = &[_]CustomerItem{CustomerItem{
    .c_custkey = 1,
    .c_name = "Alice",
    .c_acctbal = 100.0,
    .c_nationkey = 1,
    .c_address = "123 St",
    .c_phone = "123-456",
    .c_comment = "Loyal",
}}; // []const Customeritem
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
    o_orderdate: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 1000,
    .o_custkey = 1,
    .o_orderdate = "1993-10-15",
},
    OrdersItem{
    .o_orderkey = 2000,
    .o_custkey = 1,
    .o_orderdate = "1994-01-02",
},
}; // []const Ordersitem
const LineitemItem = struct {
    l_orderkey: i32,
    l_returnflag: []const u8,
    l_extendedprice: f64,
    l_discount: f64,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 1000,
    .l_returnflag = "R",
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
},
    LineitemItem{
    .l_orderkey = 2000,
    .l_returnflag = "N",
    .l_extendedprice = 500.0,
    .l_discount = 0.0,
},
}; // []const Lineitemitem
const start_date = "1993-10-01"; // []const u8
const end_date = "1994-01-01"; // []const u8
const result = blk2: { var _tmp4 = std.ArrayList(struct { key: struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
}, Items: std.ArrayList(CustomerItem) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
}, usize).init(std.heap.page_allocator); for (customer) |c| { for (orders) |o| { if (!((o.o_custkey == c.c_custkey))) continue; for (lineitem) |l| { if (!((l.l_orderkey == o.o_orderkey))) continue; for (nation) |n| { if (!((n.n_nationkey == c.c_nationkey))) continue; if (!((((o.o_orderdate >= start_date) and (o.o_orderdate < end_date)) and std.mem.eql(u8, l.l_returnflag, "R")))) continue; const _tmp6 = struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
}{
    .c_custkey = c.c_custkey,
    .c_name = c.c_name,
    .c_acctbal = c.c_acctbal,
    .c_address = c.c_address,
    .c_phone = c.c_phone,
    .c_comment = c.c_comment,
    .n_name = n.n_name,
}; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(c) catch |err| handleError(err); } else { var g = struct { key: struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
}, Items: std.ArrayList(CustomerItem) }{ .key = _tmp6, .Items = std.ArrayList(CustomerItem).init(std.heap.page_allocator) }; g.Items.append(c) catch |err| handleError(err); _tmp4.append(g) catch |err| handleError(err); _tmp5.put(_tmp6, _tmp4.items.len - 1) catch |err| handleError(err); } } } } } var _tmp7 = std.ArrayList(struct { key: struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
}, Items: std.ArrayList(CustomerItem) }).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(g) catch |err| handleError(err); } var _tmp8 = std.ArrayList(struct { item: struct { key: struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
}, Items: std.ArrayList(CustomerItem) }, key: i32 }).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp8.append(.{ .item = g, .key = -_sum_int(blk1: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch |err| handleError(err); } const _tmp3 = _tmp2.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp3; }) }) catch |err| handleError(err); } for (0.._tmp8.items.len) |i| { for (i+1.._tmp8.items.len) |j| { if (_tmp8.items[j].key < _tmp8.items[i].key) { const t = _tmp8.items[i]; _tmp8.items[i] = _tmp8.items[j]; _tmp8.items[j] = t; } } } var _tmp9 = std.ArrayList(struct { key: struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
    n_name: []const u8,
}, Items: std.ArrayList(CustomerItem) }).init(std.heap.page_allocator);for (_tmp8.items) |p| { _tmp9.append(p.item) catch |err| handleError(err); } var _tmp10 = std.ArrayList(struct {
    c_custkey: i32,
    c_name: i32,
    revenue: i32,
    c_acctbal: i32,
    n_name: i32,
    c_address: i32,
    c_phone: i32,
    c_comment: i32,
}).init(std.heap.page_allocator);for (_tmp9.items) |g| { _tmp10.append(struct {
    c_custkey: i32,
    c_name: i32,
    revenue: i32,
    c_acctbal: i32,
    n_name: i32,
    c_address: i32,
    c_phone: i32,
    c_comment: i32,
}{
    .c_custkey = g.key.c_custkey,
    .c_name = g.key.c_name,
    .revenue = _sum_int(blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }),
    .c_acctbal = g.key.c_acctbal,
    .n_name = g.key.n_name,
    .c_address = g.key.c_address,
    .c_phone = g.key.c_phone,
    .c_comment = g.key.c_comment,
}) catch |err| handleError(err); } const _tmp10Slice = _tmp10.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp10Slice; }; // []const i32

pub fn main() void {
    std.debug.print("{any}\n", .{result});
}
