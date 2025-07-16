const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _avg_int(v: []const i32) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| { sum += @floatFromInt(it); }
    return sum / @as(f64, @floatFromInt(v.len));
}

fn _contains_list_string(v: []const []const u8, item: []const u8) bool {
    for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }
    return false;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const StoreSale = struct {
    ss_sold_date_sk: i32,
    ss_item_sk: i32,
    ss_customer_sk: i32,
    ss_ticket_number: i32,
    ss_quantity: i32,
    ss_store_sk: i32,
};

const StoreReturn = struct {
    sr_returned_date_sk: i32,
    sr_customer_sk: i32,
    sr_item_sk: i32,
    sr_ticket_number: i32,
    sr_return_quantity: i32,
};

const CatalogSale = struct {
    cs_sold_date_sk: i32,
    cs_item_sk: i32,
    cs_bill_customer_sk: i32,
    cs_quantity: i32,
};

const DateDim = struct {
    d_date_sk: i32,
    d_quarter_name: []const u8,
};

const Store = struct {
    s_store_sk: i32,
    s_state: []const u8,
};

const Item = struct {
    i_item_sk: i32,
    i_item_id: []const u8,
    i_item_desc: []const u8,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var joined: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q17_stats() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "I1") catch unreachable; m.put("i_item_desc", "Item 1") catch unreachable; m.put("s_state", "CA") catch unreachable; m.put("store_sales_quantitycount", @as(i32,@intCast(1))) catch unreachable; m.put("store_sales_quantityave", 10) catch unreachable; m.put("store_sales_quantitystdev", 0) catch unreachable; m.put("store_sales_quantitycov", 0) catch unreachable; m.put("store_returns_quantitycount", @as(i32,@intCast(1))) catch unreachable; m.put("store_returns_quantityave", 2) catch unreachable; m.put("store_returns_quantitystdev", 0) catch unreachable; m.put("store_returns_quantitycov", 0) catch unreachable; m.put("catalog_sales_quantitycount", @as(i32,@intCast(1))) catch unreachable; m.put("catalog_sales_quantityave", 5) catch unreachable; m.put("catalog_sales_quantitystdev", 0) catch unreachable; m.put("catalog_sales_quantitycov", 0) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(10))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }};
    store_returns = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_returned_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("sr_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("sr_return_quantity", @as(i32,@intCast(2))) catch unreachable; break :blk2 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(5))) catch unreachable; break :blk3 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_quarter_name", "1998Q1") catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_quarter_name", "1998Q2") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("d_quarter_name", "1998Q3") catch unreachable; break :blk6 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_state", "CA") catch unreachable; break :blk7 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "I1") catch unreachable; m.put("i_item_desc", "Item 1") catch unreachable; break :blk8 m; }};
    joined = blk10: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (store_returns) |sr| { if (!((((ss.ss_customer_sk == sr.sr_customer_sk) and (ss.ss_item_sk == sr.sr_item_sk)) and (ss.ss_ticket_number == sr.sr_ticket_number)))) continue; for (catalog_sales) |cs| { if (!(((sr.sr_customer_sk == cs.cs_bill_customer_sk) and (sr.sr_item_sk == cs.cs_item_sk)))) continue; for (date_dim) |d1| { if (!(((ss.ss_sold_date_sk == d1.d_date_sk) and std.mem.eql(u8, d1.d_quarter_name, "1998Q1")))) continue; for (date_dim) |d2| { if (!(((sr.sr_returned_date_sk == d2.d_date_sk) and _contains_list_string(&[_][]const u8{"1998Q1", "1998Q2", "1998Q3"}, d2.d_quarter_name)))) continue; for (date_dim) |d3| { if (!(((cs.cs_sold_date_sk == d3.d_date_sk) and _contains_list_string(&[_][]const u8{"1998Q1", "1998Q2", "1998Q3"}, d3.d_quarter_name)))) continue; for (store) |s| { if (!((ss.ss_store_sk == s.s_store_sk))) continue; for (item) |i| { if (!((ss.ss_item_sk == i.i_item_sk))) continue; _tmp0.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("qty", ss.ss_quantity) catch unreachable; m.put("ret", sr.sr_return_quantity) catch unreachable; m.put("csq", cs.cs_quantity) catch unreachable; m.put("i_item_id", i.i_item_id) catch unreachable; m.put("i_item_desc", i.i_item_desc) catch unreachable; m.put("s_state", s.s_state) catch unreachable; break :blk9 m; }) catch unreachable; } } } } } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk10 _tmp1; };
    result = blk19: { var _tmp14 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp15 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (joined) |j| { const _tmp16 = blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", j.i_item_id) catch unreachable; m.put("i_item_desc", j.i_item_desc) catch unreachable; m.put("s_state", j.s_state) catch unreachable; break :blk11 m; }; if (_tmp15.get(_tmp16)) |idx| { _tmp14.items[idx].Items.append(j) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp16, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(j) catch unreachable; _tmp14.append(g) catch unreachable; _tmp15.put(_tmp16, _tmp14.items.len - 1) catch unreachable; } } var _tmp17 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp14.items) |g| { _tmp17.append(blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.key.i_item_id) catch unreachable; m.put("i_item_desc", g.key.i_item_desc) catch unreachable; m.put("s_state", g.key.s_state) catch unreachable; m.put("store_sales_quantitycount", (blk13: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp2.append(_) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk13 _tmp3; }).len) catch unreachable; m.put("store_sales_quantityave", _avg_int(blk14: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(x.qty) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk14 _tmp5; })) catch unreachable; m.put("store_sales_quantitystdev", 0) catch unreachable; m.put("store_sales_quantitycov", 0) catch unreachable; m.put("store_returns_quantitycount", (blk15: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp6.append(_) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk15 _tmp7; }).len) catch unreachable; m.put("store_returns_quantityave", _avg_int(blk16: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp8.append(x.ret) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk16 _tmp9; })) catch unreachable; m.put("store_returns_quantitystdev", 0) catch unreachable; m.put("store_returns_quantitycov", 0) catch unreachable; m.put("catalog_sales_quantitycount", (blk17: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp10.append(_) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk17 _tmp11; }).len) catch unreachable; m.put("catalog_sales_quantityave", _avg_int(blk18: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp12.append(x.csq) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk18 _tmp13; })) catch unreachable; m.put("catalog_sales_quantitystdev", 0) catch unreachable; m.put("catalog_sales_quantitycov", 0) catch unreachable; break :blk12 m; }) catch unreachable; } break :blk19 _tmp17.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q17_stats();
}
