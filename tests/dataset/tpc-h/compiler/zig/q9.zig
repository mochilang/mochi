const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
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
    .n_name = "BRAZIL",
},
    NationItem{
    .n_nationkey = 2,
    .n_name = "CANADA",
},
}; // []const NationItem
const SupplierItem = struct {
    s_suppkey: i32,
    s_nationkey: i32,
};
const supplier = &[_]SupplierItem{
    SupplierItem{
    .s_suppkey = 100,
    .s_nationkey = 1,
},
    SupplierItem{
    .s_suppkey = 200,
    .s_nationkey = 2,
},
}; // []const SupplierItem
const PartItem = struct {
    p_partkey: i32,
    p_name: []const u8,
};
const part = &[_]PartItem{
    PartItem{
    .p_partkey = 1000,
    .p_name = "green metal box",
},
    PartItem{
    .p_partkey = 2000,
    .p_name = "red plastic crate",
},
}; // []const PartItem
const PartsuppItem = struct {
    ps_partkey: i32,
    ps_suppkey: i32,
    ps_supplycost: f64,
};
const partsupp = &[_]PartsuppItem{
    PartsuppItem{
    .ps_partkey = 1000,
    .ps_suppkey = 100,
    .ps_supplycost = 10.0,
},
    PartsuppItem{
    .ps_partkey = 1000,
    .ps_suppkey = 200,
    .ps_supplycost = 8.0,
},
}; // []const PartsuppItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_orderdate: []const u8,
};
const orders = &[_]OrdersItem{
    OrdersItem{
    .o_orderkey = 1,
    .o_orderdate = "1995-02-10",
},
    OrdersItem{
    .o_orderkey = 2,
    .o_orderdate = "1997-01-01",
},
}; // []const OrdersItem
const LineitemItem = struct {
    l_orderkey: i32,
    l_partkey: i32,
    l_suppkey: i32,
    l_quantity: i32,
    l_extendedprice: f64,
    l_discount: f64,
};
const lineitem = &[_]LineitemItem{
    LineitemItem{
    .l_orderkey = 1,
    .l_partkey = 1000,
    .l_suppkey = 100,
    .l_quantity = 5,
    .l_extendedprice = 1000.0,
    .l_discount = 0.1,
},
    LineitemItem{
    .l_orderkey = 2,
    .l_partkey = 1000,
    .l_suppkey = 200,
    .l_quantity = 10,
    .l_extendedprice = 800.0,
    .l_discount = 0.05,
},
}; // []const LineitemItem
const prefix = "green"; // []const u8
const start_date = "1995-01-01"; // []const u8
const end_date = "1996-12-31"; // []const u8
const ResultStruct0 = struct {
    nation: []const u8,
    o_year: i32,
};
const ResultItem = struct {
    nation: i32,
    o_year: i32,
    profit: i32,
};
const ResultStruct6 = struct {
    l: LineitemItem,
    p: PartItem,
    s: SupplierItem,
    ps: PartsuppItem,
    o: OrdersItem,
    n: NationItem,
};
const ResultStruct7 = struct { key: ResultStruct0, Items: std.ArrayList(ResultStruct6) };
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q9_computes_profit_for_green_parts_by_nation_and_year() void {
    const revenue = (1000.0 * 0.9); // f64
    const cost = (5 * 10.0); // f64
    expect((result == &[_]ResultItem{ResultItem{
    .nation = "BRAZIL",
    .o_year = "1995",
    .profit = (revenue - cost),
}}));
}

pub fn main() void {
    result = blk2: { var _tmp8 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator); for (lineitem) |l| { for (part) |p| { if (!((p.p_partkey == l.l_partkey))) continue; for (supplier) |s| { if (!((s.s_suppkey == l.l_suppkey))) continue; for (partsupp) |ps| { if (!(((ps.ps_partkey == l.l_partkey) and (ps.ps_suppkey == l.l_suppkey)))) continue; for (orders) |o| { if (!((o.o_orderkey == l.l_orderkey))) continue; for (nation) |n| { if (!((n.n_nationkey == s.s_nationkey))) continue; if (!(((std.mem.eql(u8, substring(p.p_name, 0, (prefix).len), prefix) and std.mem.order(u8, o.o_orderdate, start_date) != .lt) and std.mem.order(u8, o.o_orderdate, end_date) != .gt))) continue; const _tmp9 = ResultStruct0{
    .nation = n.n_name,
    .o_year = @as(i32, substring(o.o_orderdate, 0, 4)),
}; var _found = false; var _idx: usize = 0; for (_tmp8.items, 0..) |it, i| { if (_equal(it.key, _tmp9)) { _found = true; _idx = i; break; } } if (_found) { _tmp8.items[_idx].Items.append(ResultStruct6{ .l = l, .p = p, .s = s, .ps = ps, .o = o, .n = n }) catch |err| handleError(err); } else { var g = ResultStruct7{ .key = _tmp9, .Items = std.ArrayList(ResultStruct6).init(std.heap.page_allocator) }; g.Items.append(ResultStruct6{ .l = l, .p = p, .s = s, .ps = ps, .o = o, .n = n }) catch |err| handleError(err); _tmp8.append(g) catch |err| handleError(err); } } } } } } } var _tmp10 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp10.append(g) catch |err| handleError(err); } var _tmp11 = std.ArrayList(struct { item: ResultStruct7, key: []const i32 }).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp11.append(.{ .item = g, .key = &[_][]const u8{
    g.key.nation,
    -g.key.o_year,
} }) catch |err| handleError(err); } for (0.._tmp11.items.len) |i| { for (i+1.._tmp11.items.len) |j| { if (_tmp11.items[j].key < _tmp11.items[i].key) { const t = _tmp11.items[i]; _tmp11.items[i] = _tmp11.items[j]; _tmp11.items[j] = t; } } } var _tmp12 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator);for (_tmp11.items) |p| { _tmp12.append(p.item) catch |err| handleError(err); } var _tmp13 = std.ArrayList(struct {
    nation: []const u8,
    o_year: []const u8,
    profit: i32,
}).init(std.heap.page_allocator);for (_tmp12.items) |g| { _tmp13.append(ResultItem{
    .nation = g.key.nation,
    .o_year = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{g.key.o_year}) catch |err| handleError(err),
    .profit = _sum_int(blk1: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.Items.items) |x| { _tmp4.append((((x.l.l_extendedprice * ((1 - x.l.l_discount)))) - ((x.ps.ps_supplycost * x.l.l_quantity)))) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp5; }),
}) catch |err| handleError(err); } const _tmp13Slice = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp13Slice; };
    _json(result);
    test_Q9_computes_profit_for_green_parts_by_nation_and_year();
}
