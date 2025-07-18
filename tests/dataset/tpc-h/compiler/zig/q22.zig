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

fn _contains_list_string(v: []const []const u8, item: []const u8) bool {
    for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }
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
const avg_balance = _avg_float(blk0: { var _tmp0 = std.ArrayList(f64).init(std.heap.page_allocator); for (customer) |c| { if (!(((c.c_acctbal > 0.0) and _contains_list_string(valid_codes, substring(c.c_phone, 0, 2))))) continue; _tmp0.append(c.c_acctbal) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }); // f64
const EligibleCustomersItem = struct {
    cntrycode: []const u8,
    c_acctbal: f64,
};
var eligible_customers: []const EligibleCustomersItem = undefined; // []const EligibleCustomersItem
const ResultStruct7 = struct { key: i32, Items: std.ArrayList(EligibleCustomersItem) };
var groups: []const i32 = undefined; // []const i32
var tmp = &[]i32{}; // []const i32
var result: []const i32 = undefined; // []const i32

fn test_Q22_returns_wealthy_inactive_customers_by_phone_prefix() void {
    expect((result == (blk5: { const _tmp15 = struct {
    cntrycode: []const u8,
    numcust: i32,
    totacctbal: f64,
}; const _arr = &[_]_tmp15{
    _tmp15{
    .cntrycode = "13",
    .numcust = 1,
    .totacctbal = 600.0,
},
    _tmp15{
    .cntrycode = "30",
    .numcust = 1,
    .totacctbal = 700.0,
},
}; break :blk5 _arr; })));
}

pub fn main() void {
    eligible_customers = blk2: { var _tmp5 = std.ArrayList(EligibleCustomersItem).init(std.heap.page_allocator); for (customer) |c| { if (!(((_contains_list_string(valid_codes, substring(c.c_phone, 0, 2)) and (c.c_acctbal > avg_balance)) and (!(blk1: { var _tmp3 = std.ArrayList(OrdersItem).init(std.heap.page_allocator); for (orders) |o| { if (!((o.o_custkey == c.c_custkey))) continue; _tmp3.append(o) catch |err| handleError(err); } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; }).len != 0)))) continue; _tmp5.append(EligibleCustomersItem{
    .cntrycode = substring(c.c_phone, 0, 2),
    .c_acctbal = c.c_acctbal,
}) catch |err| handleError(err); } const _tmp6 = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp6; };
    groups = blk3: { var _tmp8 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator); for (eligible_customers) |c| { const _tmp9 = c.cntrycode; var _found = false; var _idx: usize = 0; for (_tmp8.items, 0..) |it, i| { if (_equal(it.key, _tmp9)) { _found = true; _idx = i; break; } } if (_found) { _tmp8.items[_idx].Items.append(c) catch |err| handleError(err); } else { var g = ResultStruct7{ .key = _tmp9, .Items = std.ArrayList(EligibleCustomersItem).init(std.heap.page_allocator) }; g.Items.append(c) catch |err| handleError(err); _tmp8.append(g) catch |err| handleError(err); } } var _tmp10 = std.ArrayList(ResultStruct7).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp10.append(g) catch |err| handleError(err); } var _tmp11 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp11.append(g) catch |err| handleError(err); } const _tmp11Slice = _tmp11.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp11Slice; };
    for (groups) |g| {
        const total = _sum_int(blk6: { var _tmp16 = std.ArrayList(i32).init(std.heap.page_allocator); for (g.items) |x| { _tmp16.append(x.c_acctbal) catch |err| handleError(err); } const _tmp17 = _tmp16.toOwnedSlice() catch |err| handleError(err); break :blk6 _tmp17; }); // f64
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
        tmp = blk7: { var _tmp18 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp18.deinit(); _tmp18.appendSlice(tmp) catch |err| handleError(err); _tmp18.append(row) catch |err| handleError(err); const res = _tmp18.toOwnedSlice() catch |err| handleError(err); break :blk7 res; };
    }
    result = blk4: { var _tmp12 = std.ArrayList(struct { item: i32, key: i32 }).init(std.heap.page_allocator); for (tmp) |r| { _tmp12.append(.{ .item = r, .key = r.cntrycode }) catch |err| handleError(err); } for (0.._tmp12.items.len) |i| { for (i+1.._tmp12.items.len) |j| { if (_tmp12.items[j].key < _tmp12.items[i].key) { const t = _tmp12.items[i]; _tmp12.items[i] = _tmp12.items[j]; _tmp12.items[j] = t; } } } var _tmp13 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp12.items) |p| { _tmp13.append(p.item) catch |err| handleError(err); } const _tmp14 = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp14; };
    _json(result);
    test_Q22_returns_wealthy_inactive_customers_by_phone_prefix();
}
