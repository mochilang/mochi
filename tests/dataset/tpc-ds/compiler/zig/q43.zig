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

var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var year: i32 = undefined;
var gmt: i32 = undefined;
var records: []const std.AutoHashMap([]const u8, i32) = undefined;
var base: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q43_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("s_store_name", "Main") catch unreachable; m.put("s_store_id", "S1") catch unreachable; m.put("sun_sales", 10) catch unreachable; m.put("mon_sales", 20) catch unreachable; m.put("tue_sales", 30) catch unreachable; m.put("wed_sales", 40) catch unreachable; m.put("thu_sales", 50) catch unreachable; m.put("fri_sales", 60) catch unreachable; m.put("sat_sales", 70) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_day_name", "Sunday") catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_day_name", "Monday") catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("d_day_name", "Tuesday") catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(4))) catch unreachable; m.put("d_day_name", "Wednesday") catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(5))) catch unreachable; m.put("d_day_name", "Thursday") catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(6))) catch unreachable; m.put("d_day_name", "Friday") catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("date_sk", @as(i32,@intCast(7))) catch unreachable; m.put("d_day_name", "Saturday") catch unreachable; m.put("d_year", @as(i32,@intCast(2020))) catch unreachable; break :blk7 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("store_id", "S1") catch unreachable; m.put("store_name", "Main") catch unreachable; m.put("gmt_offset", @as(i32,@intCast(0))) catch unreachable; break :blk8 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 10) catch unreachable; break :blk9 m; }, blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(2))) catch unreachable; m.put("store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 20) catch unreachable; break :blk10 m; }, blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(3))) catch unreachable; m.put("store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 30) catch unreachable; break :blk11 m; }, blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(4))) catch unreachable; m.put("store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 40) catch unreachable; break :blk12 m; }, blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(5))) catch unreachable; m.put("store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 50) catch unreachable; break :blk13 m; }, blk14: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(6))) catch unreachable; m.put("store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 60) catch unreachable; break :blk14 m; }, blk15: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sold_date_sk", @as(i32,@intCast(7))) catch unreachable; m.put("store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sales_price", 70) catch unreachable; break :blk15 m; }};
    year = @as(i32,@intCast(2020));
    gmt = @as(i32,@intCast(0));
    records = blk17: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (date_dim) |d| { for (store_sales) |ss| { if (!((ss.sold_date_sk == d.date_sk))) continue; for (store) |s| { if (!((ss.store_sk == s.store_sk))) continue; if (!(((s.gmt_offset == gmt) and (d.d_year == year)))) continue; _tmp0.append(blk16: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_day_name", d.d_day_name) catch unreachable; m.put("s_store_name", s.store_name) catch unreachable; m.put("s_store_id", s.store_id) catch unreachable; m.put("price", ss.sales_price) catch unreachable; break :blk16 m; }) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk17 _tmp1; };
    base = blk27: { var _tmp16 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp17 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (records) |r| { const _tmp18 = blk18: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("name", r.s_store_name) catch unreachable; m.put("id", r.s_store_id) catch unreachable; break :blk18 m; }; if (_tmp17.get(_tmp18)) |idx| { _tmp16.items[idx].Items.append(r) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp18, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(r) catch unreachable; _tmp16.append(g) catch unreachable; _tmp17.put(_tmp18, _tmp16.items.len - 1) catch unreachable; } } var _tmp19 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp16.items) |g| { _tmp19.append(blk19: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_name", g.key.name) catch unreachable; m.put("s_store_id", g.key.id) catch unreachable; m.put("sun_sales", _sum_int(blk20: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(if (std.mem.eql(u8, x.d_day_name, "Sunday")) (x.price) else (0)) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk20 _tmp3; })) catch unreachable; m.put("mon_sales", _sum_int(blk21: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(if (std.mem.eql(u8, x.d_day_name, "Monday")) (x.price) else (0)) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk21 _tmp5; })) catch unreachable; m.put("tue_sales", _sum_int(blk22: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(if (std.mem.eql(u8, x.d_day_name, "Tuesday")) (x.price) else (0)) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk22 _tmp7; })) catch unreachable; m.put("wed_sales", _sum_int(blk23: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp8.append(if (std.mem.eql(u8, x.d_day_name, "Wednesday")) (x.price) else (0)) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk23 _tmp9; })) catch unreachable; m.put("thu_sales", _sum_int(blk24: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp10.append(if (std.mem.eql(u8, x.d_day_name, "Thursday")) (x.price) else (0)) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk24 _tmp11; })) catch unreachable; m.put("fri_sales", _sum_int(blk25: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp12.append(if (std.mem.eql(u8, x.d_day_name, "Friday")) (x.price) else (0)) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk25 _tmp13; })) catch unreachable; m.put("sat_sales", _sum_int(blk26: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp14.append(if (std.mem.eql(u8, x.d_day_name, "Saturday")) (x.price) else (0)) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk26 _tmp15; })) catch unreachable; break :blk19 m; }) catch unreachable; } break :blk27 _tmp19.toOwnedSlice() catch unreachable; };
    result = base;
    _json(result);
    test_TPCDS_Q43_simplified();
}
