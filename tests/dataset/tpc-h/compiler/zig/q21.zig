const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
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

const NationItem = struct {
    n_nationkey: i32,
    n_name: []const u8,
};
const nation = &[_]NationItem{
    NationItem{
    .n_nationkey = 1,
    .n_name = "SAUDI ARABIA",
},
    NationItem{
    .n_nationkey = 2,
    .n_name = "FRANCE",
},
}; // []const NationItem
const SupplierItem = struct {
    s_suppkey: i32,
    s_name: []const u8,
    s_nationkey: i32,
};
const supplier = &[_]SupplierItem{
    SupplierItem{
    .s_suppkey = 100,
    .s_name = "Desert Trade",
    .s_nationkey = 1,
},
    SupplierItem{
    .s_suppkey = 200,
    .s_name = "Euro Goods",
    .s_nationkey = 2,
},
}; // []const SupplierItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_orderstatus: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 500,
    .o_orderstatus = "F",
},
    OrdersItem{
    .o_orderkey = 600,
    .o_orderstatus = "O",
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_suppkey: i32,
    l_receiptdate: []const u8,
    l_commitdate: []const u8,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 500,
    .l_suppkey = 100,
    .l_receiptdate = "1995-04-15",
    .l_commitdate = "1995-04-10",
},
    LineitemItem{
    .l_orderkey = 500,
    .l_suppkey = 200,
    .l_receiptdate = "1995-04-12",
    .l_commitdate = "1995-04-12",
},
    LineitemItem{
    .l_orderkey = 600,
    .l_suppkey = 100,
    .l_receiptdate = "1995-05-01",
    .l_commitdate = "1995-04-25",
},
}; // []const LineitemItem
const ResultItem = struct {
    s_name: []const u8,
    numwait: i32,
};
const ResultStruct3 = struct {
    s: SupplierItem,
    l1: LineitemItem,
    o: OrdersItem,
    n: NationItem,
};
const ResultStruct4 = struct { key: []const u8, Items: std.ArrayList(ResultStruct3) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q21_returns_Saudi_suppliers_who_caused_unique_delivery_delays() void {
    expect((result == &[_]ResultItem{ResultItem{
    .s_name = "Desert Trade",
    .numwait = 1,
}}));
}

pub fn main() void {
    result = blk1: { var _tmp5 = std.ArrayList(ResultStruct4).init(std.heap.page_allocator); for (supplier) |s| { for (lineitem) |l1| { if (!((s.s_suppkey == l1.l_suppkey))) continue; for (orders) |o| { if (!((o.o_orderkey == l1.l_orderkey))) continue; for (nation) |n| { if (!((n.n_nationkey == s.s_nationkey))) continue; if (!((((std.mem.eql(u8, o.o_orderstatus, "F") and std.mem.order(u8, l1.l_receiptdate, l1.l_commitdate) == .gt) and std.mem.eql(u8, n.n_name, "SAUDI ARABIA")) and (!(blk0: { var _tmp1 = std.ArrayList(LineitemItem).init(std.heap.page_allocator); for (lineitem) |x| { if (!((((x.l_orderkey == l1.l_orderkey) and (x.l_suppkey != l1.l_suppkey)) and std.mem.order(u8, x.l_receiptdate, x.l_commitdate) == .gt))) continue; _tmp1.append(x) catch |err| handleError(err); } const _tmp2 = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp2; }).len != 0)))) continue; const _tmp6 = s.s_name; var _found = false; var _idx: usize = 0; for (_tmp5.items, 0..) |it, i| { if (_equal(it.key, _tmp6)) { _found = true; _idx = i; break; } } if (_found) { _tmp5.items[_idx].Items.append(ResultStruct3{ .s = s, .l1 = l1, .o = o, .n = n }) catch |err| handleError(err); } else { var g = ResultStruct4{ .key = _tmp6, .Items = std.ArrayList(ResultStruct3).init(std.heap.page_allocator) }; g.Items.append(ResultStruct3{ .s = s, .l1 = l1, .o = o, .n = n }) catch |err| handleError(err); _tmp5.append(g) catch |err| handleError(err); } } } } } var _tmp7 = std.ArrayList(ResultStruct4).init(std.heap.page_allocator);for (_tmp5.items) |g| { _tmp7.append(g) catch |err| handleError(err); } var _tmp8 = std.ArrayList(struct { item: ResultStruct4, key: []const i32 }).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp8.append(.{ .item = g, .key = &[_]i32{
    -@as(i32, @intCast(g.Items.items.len)),
    g.key,
} }) catch |err| handleError(err); } for (0.._tmp8.items.len) |i| { for (i+1.._tmp8.items.len) |j| { if (_tmp8.items[j].key < _tmp8.items[i].key) { const t = _tmp8.items[i]; _tmp8.items[i] = _tmp8.items[j]; _tmp8.items[j] = t; } } } var _tmp9 = std.ArrayList(ResultStruct4).init(std.heap.page_allocator);for (_tmp8.items) |p| { _tmp9.append(p.item) catch |err| handleError(err); } var _tmp10 = std.ArrayList(ResultItem).init(std.heap.page_allocator);for (_tmp9.items) |g| { _tmp10.append(ResultItem{
    .s_name = g.key,
    .numwait = @as(i32, @intCast(g.Items.items.len)),
}) catch |err| handleError(err); } const _tmp10Slice = _tmp10.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp10Slice; };
    _json(result);
    test_Q21_returns_Saudi_suppliers_who_caused_unique_delivery_delays();
}
