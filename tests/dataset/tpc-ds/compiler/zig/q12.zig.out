const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
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

const WebSale = struct {
    ws_item_sk: i32,
    ws_sold_date_sk: i32,
    ws_ext_sales_price: f64,
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

var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var filtered: []const std.AutoHashMap([]const u8, i32) = undefined;
var class_totals: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q12_revenue_ratio() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "ITEM1") catch unreachable; m.put("i_item_desc", "Item One") catch unreachable; m.put("i_category", "A") catch unreachable; m.put("i_class", "C1") catch unreachable; m.put("i_current_price", 10) catch unreachable; m.put("itemrevenue", 200) catch unreachable; m.put("revenueratio", 50) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "ITEM2") catch unreachable; m.put("i_item_desc", "Item Two") catch unreachable; m.put("i_category", "A") catch unreachable; m.put("i_class", "C1") catch unreachable; m.put("i_current_price", 20) catch unreachable; m.put("itemrevenue", 200) catch unreachable; m.put("revenueratio", 50) catch unreachable; break :blk1 m; }}));
}

pub fn main() void {
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_sales_price", 100) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_ext_sales_price", 100) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_ext_sales_price", 200) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_item_sk", @as(i32,@intCast(3))) catch unreachable; m.put("ws_sold_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("ws_ext_sales_price", 50) catch unreachable; break :blk5 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "ITEM1") catch unreachable; m.put("i_item_desc", "Item One") catch unreachable; m.put("i_category", "A") catch unreachable; m.put("i_class", "C1") catch unreachable; m.put("i_current_price", 10) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_item_id", "ITEM2") catch unreachable; m.put("i_item_desc", "Item Two") catch unreachable; m.put("i_category", "A") catch unreachable; m.put("i_class", "C1") catch unreachable; m.put("i_current_price", 20) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(3))) catch unreachable; m.put("i_item_id", "ITEM3") catch unreachable; m.put("i_item_desc", "Item Three") catch unreachable; m.put("i_category", "B") catch unreachable; m.put("i_class", "C2") catch unreachable; m.put("i_current_price", 30) catch unreachable; break :blk8 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_date", "2001-01-20") catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_date", "2001-02-05") catch unreachable; break :blk10 m; }, blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("d_date", "2001-03-05") catch unreachable; break :blk11 m; }};
    filtered = blk15: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (web_sales) |ws| { for (item) |i| { if (!((ws.ws_item_sk == i.i_item_sk))) continue; for (date_dim) |d| { if (!((ws.ws_sold_date_sk == d.d_date_sk))) continue; if (!(((_contains_list_string(&[_][]const u8{"A", "B", "C"}, i.i_category) and (d.d_date >= "2001-01-15")) and (d.d_date <= "2001-02-14")))) continue; const _tmp4 = blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", i.i_item_id) catch unreachable; m.put("desc", i.i_item_desc) catch unreachable; m.put("cat", i.i_category) catch unreachable; m.put("class", i.i_class) catch unreachable; m.put("price", i.i_current_price) catch unreachable; break :blk12 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(ws) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ws) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.key.id) catch unreachable; m.put("i_item_desc", g.key.desc) catch unreachable; m.put("i_category", g.key.cat) catch unreachable; m.put("i_class", g.key.class) catch unreachable; m.put("i_current_price", g.key.price) catch unreachable; m.put("itemrevenue", _sum_int(blk14: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ws_ext_sales_price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk14 _tmp1; })) catch unreachable; break :blk13 m; }) catch unreachable; } break :blk15 _tmp5.toOwnedSlice() catch unreachable; };
    class_totals = blk18: { var _tmp8 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (filtered) |f| { const _tmp10 = f.i_class; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(f) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(f) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk16: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("class", g.key) catch unreachable; m.put("total", _sum_int(blk17: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.itemrevenue) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk17 _tmp7; })) catch unreachable; break :blk16 m; }) catch unreachable; } break :blk18 _tmp11.toOwnedSlice() catch unreachable; };
    result = blk20: { var _tmp12 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (filtered) |f| { for (class_totals) |t| { if (!((f.i_class == t.class))) continue; _tmp12.append(blk19: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", f.i_item_id) catch unreachable; m.put("i_item_desc", f.i_item_desc) catch unreachable; m.put("i_category", f.i_category) catch unreachable; m.put("i_class", f.i_class) catch unreachable; m.put("i_current_price", f.i_current_price) catch unreachable; m.put("itemrevenue", f.itemrevenue) catch unreachable; m.put("revenueratio", (((f.itemrevenue * 100)) / t.total)) catch unreachable; break :blk19 m; }) catch unreachable; } } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk20 _tmp13; };
    _json(result);
    test_TPCDS_Q12_revenue_ratio();
}
