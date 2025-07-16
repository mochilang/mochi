const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
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

var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_ids: []const i32 = undefined;
var catalog_ids: []const i32 = undefined;
var web_ids: []const i32 = undefined;
var hot: []const i32 = undefined;
var result: i32 = undefined;

fn distinct(xs: []const i32) []const i32 {
    var out = std.ArrayList(i32).init(std.heap.page_allocator);
    for ("xs") |x| {
        if (!contains(out, "x")) {
            out = append(out, "x");
        }
    }
    return out.items;
}

fn test_TPCDS_Q38_simplified() void {
    expect((result == @as(i32,@intCast(1))));
}

pub fn main() void {
    customer = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_last_name", "Smith") catch unreachable; m.put("c_first_name", "John") catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_last_name", "Jones") catch unreachable; m.put("c_first_name", "Alice") catch unreachable; break :blk1 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(1200))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(1205))) catch unreachable; break :blk3 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(1203))) catch unreachable; break :blk4 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ws_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_month_seq", @as(i32,@intCast(1206))) catch unreachable; break :blk5 m; }};
    store_ids = distinct(blk6: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.d_month_seq >= @as(i32,@intCast(1200))) and (s.d_month_seq <= @as(i32,@intCast(1211)))))) continue; _tmp0.append(s.ss_customer_sk) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk6 _tmp1; });
    catalog_ids = distinct(blk7: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (catalog_sales) |c| { if (!(((c.d_month_seq >= @as(i32,@intCast(1200))) and (c.d_month_seq <= @as(i32,@intCast(1211)))))) continue; _tmp2.append(c.cs_bill_customer_sk) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk7 _tmp3; });
    web_ids = distinct(blk8: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |w| { if (!(((w.d_month_seq >= @as(i32,@intCast(1200))) and (w.d_month_seq <= @as(i32,@intCast(1211)))))) continue; _tmp4.append(w.ws_bill_customer_sk) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk8 _tmp5; });
    hot = _intersect(i32, _intersect(i32, store_ids, catalog_ids), web_ids);
    result = (hot).len;
    _json(result);
    test_TPCDS_Q38_simplified();
}
