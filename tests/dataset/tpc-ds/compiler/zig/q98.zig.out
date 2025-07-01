const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
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
    ss_item_sk: i32,
    ss_sold_date_sk: i32,
    ss_ext_sales_price: f64,
};

const Item = struct {
    i_item_sk: i32,
    i_item_id: []const u8,
    i_item_desc: []const u8,
    i_category: []const u8,
    i_class: []const u8,
    i_current_price: f64,
};

const DateDim = struct {
    d_date_sk: i32,
    d_date: []const u8,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var grouped: []const std.AutoHashMap([]const u8, i32) = undefined;
var totals: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q98_revenue() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "I1") catch unreachable; m.put("i_item_desc", "desc1") catch unreachable; m.put("i_category", "CatA") catch unreachable; m.put("i_class", "Class1") catch unreachable; m.put("i_current_price", 100) catch unreachable; m.put("itemrevenue", 50) catch unreachable; m.put("revenueratio", 33.333333333333336) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "I2") catch unreachable; m.put("i_item_desc", "desc2") catch unreachable; m.put("i_category", "CatB") catch unreachable; m.put("i_class", "Class1") catch unreachable; m.put("i_current_price", 200) catch unreachable; m.put("itemrevenue", 100) catch unreachable; m.put("revenueratio", 66.66666666666667) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 50) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ext_sales_price", 100) catch unreachable; break :blk3 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "I1") catch unreachable; m.put("i_item_desc", "desc1") catch unreachable; m.put("i_category", "CatA") catch unreachable; m.put("i_class", "Class1") catch unreachable; m.put("i_current_price", 100) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_item_id", "I2") catch unreachable; m.put("i_item_desc", "desc2") catch unreachable; m.put("i_category", "CatB") catch unreachable; m.put("i_class", "Class1") catch unreachable; m.put("i_current_price", 200) catch unreachable; break :blk5 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_date", "2000-02-01") catch unreachable; break :blk6 m; }};
    grouped = blk10: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (item) |i| { if (!((ss.ss_item_sk == i.i_item_sk))) continue; for (date_dim) |d| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; const _tmp4 = blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("item_id", i.i_item_id) catch unreachable; m.put("item_desc", i.i_item_desc) catch unreachable; m.put("category", i.i_category) catch unreachable; m.put("class", i.i_class) catch unreachable; m.put("price", i.i_current_price) catch unreachable; break :blk7 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.key.item_id) catch unreachable; m.put("i_item_desc", g.key.item_desc) catch unreachable; m.put("i_category", g.key.category) catch unreachable; m.put("i_class", g.key.class) catch unreachable; m.put("i_current_price", g.key.price) catch unreachable; m.put("itemrevenue", _sum_int(blk9: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss_ext_sales_price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk9 _tmp1; })) catch unreachable; break :blk8 m; }) catch unreachable; } break :blk10 _tmp5.toOwnedSlice() catch unreachable; };
    totals = blk13: { var _tmp8 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (grouped) |g| { const _tmp10 = g.i_class; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(g) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(g) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |cg| { _tmp11.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("class", cg.key) catch unreachable; m.put("total", _sum_int(blk12: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (cg) |x| { _tmp6.append(x.itemrevenue) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk12 _tmp7; })) catch unreachable; break :blk11 m; }) catch unreachable; } break :blk13 _tmp11.toOwnedSlice() catch unreachable; };
    result = blk15: { var _tmp12 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (grouped) |g| { for (totals) |t| { if (!((g.i_class == t.class))) continue; _tmp12.append(blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.i_item_id) catch unreachable; m.put("i_item_desc", g.i_item_desc) catch unreachable; m.put("i_category", g.i_category) catch unreachable; m.put("i_class", g.i_class) catch unreachable; m.put("i_current_price", g.i_current_price) catch unreachable; m.put("itemrevenue", g.itemrevenue) catch unreachable; m.put("revenueratio", ((g.itemrevenue * @as(i32,@intCast(100))) / t.total)) catch unreachable; break :blk14 m; }) catch unreachable; } } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk15 _tmp13; };
    _json(result);
    test_TPCDS_Q98_revenue();
}
