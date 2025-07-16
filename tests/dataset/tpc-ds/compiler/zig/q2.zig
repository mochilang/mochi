const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _contains(comptime T: type, v: []const T, item: T) bool {
    for (v) |it| { if (std.meta.eql(it, item)) return true; }
    return false;
}

fn _union_all(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { res.append(it) catch unreachable; }
    for (b) |it| { res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _union(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { res.append(it) catch unreachable; }
    for (b) |it| { if (!_contains(T, res.items, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _except(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { if (!_contains(T, b, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
}

fn _intersect(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| { if (_contains(T, b, it) and !_contains(T, res.items, it)) res.append(it) catch unreachable; }
    return res.toOwnedSlice() catch unreachable;
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

var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var wscs: []const std.AutoHashMap([]const u8, i32) = undefined;
var wswscs: []const std.AutoHashMap([]const u8, i32) = undefined;
var year1: []const std.AutoHashMap([]const u8, i32) = undefined;
var year2: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q2_result() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_week_seq1", @as(i32,@intCast(1))) catch unreachable; m.put("sun_ratio", 0.5) catch unreachable; m.put("mon_ratio", 0.5) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ws_ext_sales_price", 5) catch unreachable; m.put("ws_sold_date_name", "Sunday") catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ws_ext_sales_price", 5) catch unreachable; m.put("ws_sold_date_name", "Monday") catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_date_sk", @as(i32,@intCast(8))) catch unreachable; m.put("ws_ext_sales_price", 10) catch unreachable; m.put("ws_sold_date_name", "Sunday") catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_sold_date_sk", @as(i32,@intCast(9))) catch unreachable; m.put("ws_ext_sales_price", 10) catch unreachable; m.put("ws_sold_date_name", "Monday") catch unreachable; break :blk4 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_ext_sales_price", 5) catch unreachable; m.put("cs_sold_date_name", "Sunday") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_ext_sales_price", 5) catch unreachable; m.put("cs_sold_date_name", "Monday") catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(8))) catch unreachable; m.put("cs_ext_sales_price", 10) catch unreachable; m.put("cs_sold_date_name", "Sunday") catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(9))) catch unreachable; m.put("cs_ext_sales_price", 10) catch unreachable; m.put("cs_sold_date_name", "Monday") catch unreachable; break :blk8 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_week_seq", @as(i32,@intCast(1))) catch unreachable; m.put("d_day_name", "Sunday") catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_week_seq", @as(i32,@intCast(1))) catch unreachable; m.put("d_day_name", "Monday") catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; break :blk10 m; }, blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(8))) catch unreachable; m.put("d_week_seq", @as(i32,@intCast(54))) catch unreachable; m.put("d_day_name", "Sunday") catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; break :blk11 m; }, blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(9))) catch unreachable; m.put("d_week_seq", @as(i32,@intCast(54))) catch unreachable; m.put("d_day_name", "Monday") catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; break :blk12 m; }};
    wscs = _union_all(i32, (blk14: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (web_sales) |ws| { _tmp0.append(blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", ws.ws_sold_date_sk) catch unreachable; m.put("sales_price", ws.ws_ext_sales_price) catch unreachable; m.put("day", ws.ws_sold_date_name) catch unreachable; break :blk13 m; }) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk14 _tmp1; }), (blk16: { var _tmp2 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (catalog_sales) |cs| { _tmp2.append(blk15: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", cs.cs_sold_date_sk) catch unreachable; m.put("sales_price", cs.cs_ext_sales_price) catch unreachable; m.put("day", cs.cs_sold_date_name) catch unreachable; break :blk15 m; }) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk16 _tmp3; }));
    wswscs = blk26: { var _tmp18 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp19 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (wscs) |w| { for (date_dim) |d| { if (!((w.sold_date_sk == d.d_date_sk))) continue; const _tmp20 = blk17: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("week_seq", d.d_week_seq) catch unreachable; break :blk17 m; }; if (_tmp19.get(_tmp20)) |idx| { _tmp18.items[idx].Items.append(w) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp20, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(w) catch unreachable; _tmp18.append(g) catch unreachable; _tmp19.put(_tmp20, _tmp18.items.len - 1) catch unreachable; } } } var _tmp21 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp18.items) |g| { _tmp21.append(blk18: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_week_seq", g.key.week_seq) catch unreachable; m.put("sun_sales", _sum_int(blk19: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(std.mem.eql(u8, x.day, "Sunday"))) continue; _tmp4.append(x.sales_price) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk19 _tmp5; })) catch unreachable; m.put("mon_sales", _sum_int(blk20: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(std.mem.eql(u8, x.day, "Monday"))) continue; _tmp6.append(x.sales_price) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk20 _tmp7; })) catch unreachable; m.put("tue_sales", _sum_int(blk21: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(std.mem.eql(u8, x.day, "Tuesday"))) continue; _tmp8.append(x.sales_price) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk21 _tmp9; })) catch unreachable; m.put("wed_sales", _sum_int(blk22: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(std.mem.eql(u8, x.day, "Wednesday"))) continue; _tmp10.append(x.sales_price) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk22 _tmp11; })) catch unreachable; m.put("thu_sales", _sum_int(blk23: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(std.mem.eql(u8, x.day, "Thursday"))) continue; _tmp12.append(x.sales_price) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk23 _tmp13; })) catch unreachable; m.put("fri_sales", _sum_int(blk24: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(std.mem.eql(u8, x.day, "Friday"))) continue; _tmp14.append(x.sales_price) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk24 _tmp15; })) catch unreachable; m.put("sat_sales", _sum_int(blk25: { var _tmp16 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { if (!(std.mem.eql(u8, x.day, "Saturday"))) continue; _tmp16.append(x.sales_price) catch unreachable; } const _tmp17 = _tmp16.toOwnedSlice() catch unreachable; break :blk25 _tmp17; })) catch unreachable; break :blk18 m; }) catch unreachable; } break :blk26 _tmp21.toOwnedSlice() catch unreachable; };
    year1 = blk27: { var _tmp22 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (wswscs) |w| { if (!((w.d_week_seq == @as(i32,@intCast(1))))) continue; _tmp22.append(w) catch unreachable; } const _tmp23 = _tmp22.toOwnedSlice() catch unreachable; break :blk27 _tmp23; };
    year2 = blk28: { var _tmp24 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (wswscs) |w| { if (!((w.d_week_seq == @as(i32,@intCast(54))))) continue; _tmp24.append(w) catch unreachable; } const _tmp25 = _tmp24.toOwnedSlice() catch unreachable; break :blk28 _tmp25; };
    result = blk30: { var _tmp26 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (year1) |y| { for (year2) |z| { if (!((y.d_week_seq == (z.d_week_seq - @as(i32,@intCast(53)))))) continue; _tmp26.append(blk29: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_week_seq1", y.d_week_seq) catch unreachable; m.put("sun_ratio", (y.sun_sales / z.sun_sales)) catch unreachable; m.put("mon_ratio", (y.mon_sales / z.mon_sales)) catch unreachable; break :blk29 m; }) catch unreachable; } } const _tmp27 = _tmp26.toOwnedSlice() catch unreachable; break :blk30 _tmp27; };
    _json(result);
    test_TPCDS_Q2_result();
}
