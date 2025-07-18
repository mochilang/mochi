const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _avg_float(v: []const f64) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| { sum += it; }
    return sum / @as(f64, @floatFromInt(v.len));
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

fn _slice_string(s: []const u8, start: i32, end: i32, step: i32) []const u8 {
    var sidx = start;
    var eidx = end;
    var stp = step;
    const n: i32 = @as(i32, @intCast(s.len));
    if (sidx < 0) sidx += n;
    if (eidx < 0) eidx += n;
    if (stp == 0) stp = 1;
    if (sidx < 0) sidx = 0;
    if (eidx > n) eidx = n;
    if (stp > 0 and eidx < sidx) eidx = sidx;
    if (stp < 0 and eidx > sidx) eidx = sidx;
    var res = std.ArrayList(u8).init(std.heap.page_allocator);
    defer res.deinit();
    var i: i32 = sidx;
    while ((stp > 0 and i < eidx) or (stp < 0 and i > eidx)) : (i += stp) {
        res.append(s[@as(usize, @intCast(i))]) catch |err| handleError(err);
    }
    return res.toOwnedSlice() catch |err| handleError(err);
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return std.meta.eql(a, b);
}

const CustomerItem = struct {
    c_custkey: i32,
    c_phone: []const u8,
    c_acctbal: f64,
};
const customer = &[_]CustomerItem{
    CustomerItem{
    .c_custkey = 1,
    .c_phone = "13-123-4567",
    .c_acctbal = 600.0,
},
    CustomerItem{
    .c_custkey = 2,
    .c_phone = "31-456-7890",
    .c_acctbal = 100.0,
},
    CustomerItem{
    .c_custkey = 3,
    .c_phone = "30-000-0000",
    .c_acctbal = 700.0,
},
}; // []const CustomerItem
const OrdersItem = struct {
    o_orderkey: i32,
    o_custkey: i32,
};
const orders = &[_]OrdersItem{OrdersItem{
    .o_orderkey = 10,
    .o_custkey = 2,
}}; // []const OrdersItem
const valid_codes = &[_][]const u8{
    "13",
    "31",
    "23",
    "29",
    "30",
    "18",
    "17",
}; // []const []const u8
const avg_balance = _avg_float(blk1: { var _tmp1 = std.ArrayList(f64).init(std.heap.page_allocator); for (customer) |c| { if (!(((c.c_acctbal > 0.0) and blk0: { var found: bool = false; for (valid_codes) |_tmp0| { if (std.mem.eql(u8, _tmp0, _slice_string(c.c_phone, 0, 2, 1))) { found = true; break; } } break :blk0 found; }))) continue; _tmp1.append(c.c_acctbal) catch |err| handleError(err); } const _tmp2 = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp2; }); // f64
const EligibleCustomersItem = struct {
    cntrycode: []const u8,
    c_acctbal: f64,
};
var eligible_customers: []const EligibleCustomersItem = undefined; // []const EligibleCustomersItem
const ResultStruct9 = struct { key: i32, Items: std.ArrayList(EligibleCustomersItem) };
var groups: []const i32 = undefined; // []const i32
var tmp = &[]i32{}; // []const i32
var result: []const i32 = undefined; // []const i32

fn test_Q22_returns_wealthy_inactive_customers_by_phone_prefix() void {
    expect((result == (blk7: { const _tmp17 = struct {
    cntrycode: []const u8,
    numcust: i32,
    totacctbal: f64,
}; const _arr = &[_]_tmp17{
    _tmp17{
    .cntrycode = "13",
    .numcust = 1,
    .totacctbal = 600.0,
},
    _tmp17{
    .cntrycode = "30",
    .numcust = 1,
    .totacctbal = 700.0,
},
}; break :blk7 _arr; })));
}

pub fn main() void {
    eligible_customers = blk4: { var _tmp7 = std.ArrayList(EligibleCustomersItem).init(std.heap.page_allocator); for (customer) |c| { if (!(((blk3: { var found: bool = false; for (valid_codes) |_tmp6| { if (std.mem.eql(u8, _tmp6, _slice_string(c.c_phone, 0, 2, 1))) { found = true; break; } } break :blk3 found; } and (c.c_acctbal > avg_balance)) and (!(blk2: { var _tmp4 = std.ArrayList(OrdersItem).init(std.heap.page_allocator); for (orders) |o| { if (!((o.o_custkey == c.c_custkey))) continue; _tmp4.append(o) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp5; }).len != 0)))) continue; _tmp7.append(EligibleCustomersItem{
    .cntrycode = _slice_string(c.c_phone, 0, 2, 1),
    .c_acctbal = c.c_acctbal,
}) catch |err| handleError(err); } const _tmp8 = _tmp7.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp8; };
    groups = blk5: { var _tmp10 = std.ArrayList(ResultStruct9).init(std.heap.page_allocator); for (eligible_customers) |c| { const _tmp11 = c.cntrycode; var _found = false; var _idx: usize = 0; for (_tmp10.items, 0..) |it, i| { if (_equal(it.key, _tmp11)) { _found = true; _idx = i; break; } } if (_found) { _tmp10.items[_idx].Items.append(c) catch |err| handleError(err); } else { var g = ResultStruct9{ .key = _tmp11, .Items = std.ArrayList(EligibleCustomersItem).init(std.heap.page_allocator) }; g.Items.append(c) catch |err| handleError(err); _tmp10.append(g) catch |err| handleError(err); } } var _tmp12 = std.ArrayList(ResultStruct9).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp12.append(g) catch |err| handleError(err); } var _tmp13 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp12.items) |g| { _tmp13.append(g) catch |err| handleError(err); } const _tmp13Slice = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk5 _tmp13Slice; };
    for (groups) |g| {
        const total = _sum_int(blk8: { var _tmp18 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.items) |x| { _tmp18.append(x.c_acctbal) catch |err| handleError(err); } const _tmp19 = _tmp18.toOwnedSlice() catch |err| handleError(err); break :blk8 _tmp19; }); // f64
        const Row = struct {
    cntrycode: i32,
    numcust: i32,
    totacctbal: f64,
};
        const row = Row{
    .cntrycode = g.key,
    .numcust = @as(i32, @intCast(g.Items.items.len)),
    .totacctbal = total,
}; // struct {
    cntrycode: i32,
    numcust: i32,
    totacctbal: f64,
}
        tmp = blk9: { var _tmp20 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp20.deinit(); _tmp20.appendSlice(tmp) catch |err| handleError(err); _tmp20.append(row) catch |err| handleError(err); const res = _tmp20.toOwnedSlice() catch |err| handleError(err); break :blk9 res; };
    }
    result = blk6: { var _tmp14 = std.ArrayList(struct { item: i32, key: i32 }).init(std.heap.page_allocator); for (tmp) |r| { _tmp14.append(.{ .item = r, .key = r.cntrycode }) catch |err| handleError(err); } for (0.._tmp14.items.len) |i| { for (i+1.._tmp14.items.len) |j| { if (_tmp14.items[j].key < _tmp14.items[i].key) { const t = _tmp14.items[i]; _tmp14.items[i] = _tmp14.items[j]; _tmp14.items[j] = t; } } } var _tmp15 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp14.items) |p| { _tmp15.append(p.item) catch |err| handleError(err); } const _tmp16 = _tmp15.toOwnedSlice() catch |err| handleError(err); break :blk6 _tmp16; };
    _json(result);
    test_Q22_returns_wealthy_inactive_customers_by_phone_prefix();
}
