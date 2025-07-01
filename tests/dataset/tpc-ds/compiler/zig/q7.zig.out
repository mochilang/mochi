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

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var promotion: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q7_result() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "I1") catch unreachable; m.put("agg1", 5) catch unreachable; m.put("agg2", 10) catch unreachable; m.put("agg3", 2) catch unreachable; m.put("agg4", 8) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_promo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(5))) catch unreachable; m.put("ss_list_price", 10) catch unreachable; m.put("ss_coupon_amt", 2) catch unreachable; m.put("ss_sales_price", 8) catch unreachable; break :blk1 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_gender", "M") catch unreachable; m.put("cd_marital_status", "S") catch unreachable; m.put("cd_education_status", "College") catch unreachable; break :blk2 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; break :blk3 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "I1") catch unreachable; break :blk4 m; }};
    promotion = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("p_promo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("p_channel_email", "N") catch unreachable; m.put("p_channel_event", "Y") catch unreachable; break :blk5 m; }};
    result = blk12: { var _tmp8 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (customer_demographics) |cd| { if (!((ss.ss_cdemo_sk == cd.cd_demo_sk))) continue; for (date_dim) |d| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; for (item) |i| { if (!((ss.ss_item_sk == i.i_item_sk))) continue; for (promotion) |p| { if (!((ss.ss_promo_sk == p.p_promo_sk))) continue; if (!(((((std.mem.eql(u8, cd.cd_gender, "M") and std.mem.eql(u8, cd.cd_marital_status, "S")) and std.mem.eql(u8, cd.cd_education_status, "College")) and ((std.mem.eql(u8, p.p_channel_email, "N") or std.mem.eql(u8, p.p_channel_event, "N")))) and (d.d_year == @as(i32,@intCast(1998)))))) continue; const _tmp10 = blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", i.i_item_id) catch unreachable; break :blk6 m; }; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } } } } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.key.i_item_id) catch unreachable; m.put("agg1", _avg_int(blk8: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss.ss_quantity) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk8 _tmp1; })) catch unreachable; m.put("agg2", _avg_int(blk9: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.ss.ss_list_price) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk9 _tmp3; })) catch unreachable; m.put("agg3", _avg_int(blk10: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(x.ss.ss_coupon_amt) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk10 _tmp5; })) catch unreachable; m.put("agg4", _avg_int(blk11: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.ss.ss_sales_price) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk11 _tmp7; })) catch unreachable; break :blk7 m; }) catch unreachable; } break :blk12 _tmp11.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q7_result();
}
