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

const StoreSale = struct {
    ss_quantity: i32,
    ss_list_price: f64,
    ss_coupon_amt: f64,
    ss_wholesale_cost: f64,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var bucket1: []const std.AutoHashMap([]const u8, i32) = undefined;
var bucket2: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q28_buckets() void {
    expect((result == blk0: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("B1_LP", 100) catch unreachable; m.put("B1_CNT", @as(i32,@intCast(1))) catch unreachable; m.put("B1_CNTD", @as(i32,@intCast(1))) catch unreachable; m.put("B2_LP", 80) catch unreachable; m.put("B2_CNT", @as(i32,@intCast(1))) catch unreachable; m.put("B2_CNTD", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", @as(i32,@intCast(3))) catch unreachable; m.put("ss_list_price", 100) catch unreachable; m.put("ss_coupon_amt", 50) catch unreachable; m.put("ss_wholesale_cost", 30) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", @as(i32,@intCast(8))) catch unreachable; m.put("ss_list_price", 80) catch unreachable; m.put("ss_coupon_amt", 10) catch unreachable; m.put("ss_wholesale_cost", 20) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", @as(i32,@intCast(12))) catch unreachable; m.put("ss_list_price", 60) catch unreachable; m.put("ss_coupon_amt", 5) catch unreachable; m.put("ss_wholesale_cost", 15) catch unreachable; break :blk3 m; }};
    bucket1 = blk4: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { if (!((((ss.ss_quantity >= @as(i32,@intCast(0))) and (ss.ss_quantity <= @as(i32,@intCast(5)))) and ((((((ss.ss_list_price >= @as(i32,@intCast(0))) and (ss.ss_list_price <= @as(i32,@intCast(110))))) or (((ss.ss_coupon_amt >= @as(i32,@intCast(0))) and (ss.ss_coupon_amt <= @as(i32,@intCast(1000)))))) or (((ss.ss_wholesale_cost >= @as(i32,@intCast(0))) and (ss.ss_wholesale_cost <= @as(i32,@intCast(50)))))))))) continue; _tmp0.append(ss) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk4 _tmp1; };
    bucket2 = blk5: { var _tmp2 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { if (!((((ss.ss_quantity >= @as(i32,@intCast(6))) and (ss.ss_quantity <= @as(i32,@intCast(10)))) and ((((((ss.ss_list_price >= @as(i32,@intCast(0))) and (ss.ss_list_price <= @as(i32,@intCast(110))))) or (((ss.ss_coupon_amt >= @as(i32,@intCast(0))) and (ss.ss_coupon_amt <= @as(i32,@intCast(1000)))))) or (((ss.ss_wholesale_cost >= @as(i32,@intCast(0))) and (ss.ss_wholesale_cost <= @as(i32,@intCast(50)))))))))) continue; _tmp2.append(ss) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk5 _tmp3; };
    result = blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("B1_LP", _avg_int(blk7: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (bucket1) |x| { _tmp4.append(x.ss_list_price) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk7 _tmp5; })) catch unreachable; m.put("B1_CNT", (bucket1).len) catch unreachable; m.put("B1_CNTD", (blk8: { var _tmp6 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp7 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (bucket1) |x| { const _tmp8 = x.ss_list_price; if (_tmp7.get(_tmp8)) |idx| { _tmp6.items[idx].Items.append(x) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp8, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(x) catch unreachable; _tmp6.append(g) catch unreachable; _tmp7.put(_tmp8, _tmp6.items.len - 1) catch unreachable; } } var _tmp9 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp6.items) |g| { _tmp9.append(g.key) catch unreachable; } break :blk8 _tmp9.toOwnedSlice() catch unreachable; }).len) catch unreachable; m.put("B2_LP", _avg_int(blk9: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (bucket2) |x| { _tmp10.append(x.ss_list_price) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk9 _tmp11; })) catch unreachable; m.put("B2_CNT", (bucket2).len) catch unreachable; m.put("B2_CNTD", (blk10: { var _tmp12 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp13 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (bucket2) |x| { const _tmp14 = x.ss_list_price; if (_tmp13.get(_tmp14)) |idx| { _tmp12.items[idx].Items.append(x) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp14, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(x) catch unreachable; _tmp12.append(g) catch unreachable; _tmp13.put(_tmp14, _tmp12.items.len - 1) catch unreachable; } } var _tmp15 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp12.items) |g| { _tmp15.append(g.key) catch unreachable; } break :blk10 _tmp15.toOwnedSlice() catch unreachable; }).len) catch unreachable; break :blk6 m; };
    _json(result);
    test_TPCDS_Q28_buckets();
}
