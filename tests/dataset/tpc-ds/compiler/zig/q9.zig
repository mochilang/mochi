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

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var reason: []const std.AutoHashMap([]const u8, i32) = undefined;
var bucket1: f64 = undefined;
var bucket2: f64 = undefined;
var bucket3: f64 = undefined;
var bucket4: f64 = undefined;
var bucket5: f64 = undefined;
var result: []const std.AutoHashMap([]const u8, f64) = undefined;

fn test_TPCDS_Q9_result() void {
    expect((result == &[_]std.AutoHashMap([]const u8, f64){blk0: { var m = std.AutoHashMap(f64, f64).init(std.heap.page_allocator); m.put(bucket1, 7) catch unreachable; m.put(bucket2, 15) catch unreachable; m.put(bucket3, 30) catch unreachable; m.put(bucket4, 35) catch unreachable; m.put(bucket5, 50) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", @as(i32,@intCast(5))) catch unreachable; m.put("ss_ext_discount_amt", 5) catch unreachable; m.put("ss_net_paid", 7) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", @as(i32,@intCast(30))) catch unreachable; m.put("ss_ext_discount_amt", 10) catch unreachable; m.put("ss_net_paid", 15) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", @as(i32,@intCast(50))) catch unreachable; m.put("ss_ext_discount_amt", 20) catch unreachable; m.put("ss_net_paid", 30) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", @as(i32,@intCast(70))) catch unreachable; m.put("ss_ext_discount_amt", 25) catch unreachable; m.put("ss_net_paid", 35) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_quantity", @as(i32,@intCast(90))) catch unreachable; m.put("ss_ext_discount_amt", 40) catch unreachable; m.put("ss_net_paid", 50) catch unreachable; break :blk5 m; }};
    reason = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("r_reason_sk", @as(i32,@intCast(1))) catch unreachable; break :blk6 m; }};
    bucket1 = if (((blk7: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(1))) and (s.ss_quantity <= @as(i32,@intCast(20)))))) continue; _tmp0.append(s) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; }).len > @as(i32,@intCast(10)))) (_avg_int(blk8: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(1))) and (s.ss_quantity <= @as(i32,@intCast(20)))))) continue; _tmp2.append(s.ss_ext_discount_amt) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk8 _tmp3; })) else (_avg_int(blk9: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(1))) and (s.ss_quantity <= @as(i32,@intCast(20)))))) continue; _tmp4.append(s.ss_net_paid) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk9 _tmp5; }));
    bucket2 = if (((blk10: { var _tmp6 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(21))) and (s.ss_quantity <= @as(i32,@intCast(40)))))) continue; _tmp6.append(s) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk10 _tmp7; }).len > @as(i32,@intCast(20)))) (_avg_int(blk11: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(21))) and (s.ss_quantity <= @as(i32,@intCast(40)))))) continue; _tmp8.append(s.ss_ext_discount_amt) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk11 _tmp9; })) else (_avg_int(blk12: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(21))) and (s.ss_quantity <= @as(i32,@intCast(40)))))) continue; _tmp10.append(s.ss_net_paid) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk12 _tmp11; }));
    bucket3 = if (((blk13: { var _tmp12 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(41))) and (s.ss_quantity <= @as(i32,@intCast(60)))))) continue; _tmp12.append(s) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk13 _tmp13; }).len > @as(i32,@intCast(30)))) (_avg_int(blk14: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(41))) and (s.ss_quantity <= @as(i32,@intCast(60)))))) continue; _tmp14.append(s.ss_ext_discount_amt) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk14 _tmp15; })) else (_avg_int(blk15: { var _tmp16 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(41))) and (s.ss_quantity <= @as(i32,@intCast(60)))))) continue; _tmp16.append(s.ss_net_paid) catch unreachable; } const _tmp17 = _tmp16.toOwnedSlice() catch unreachable; break :blk15 _tmp17; }));
    bucket4 = if (((blk16: { var _tmp18 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(61))) and (s.ss_quantity <= @as(i32,@intCast(80)))))) continue; _tmp18.append(s) catch unreachable; } const _tmp19 = _tmp18.toOwnedSlice() catch unreachable; break :blk16 _tmp19; }).len > @as(i32,@intCast(40)))) (_avg_int(blk17: { var _tmp20 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(61))) and (s.ss_quantity <= @as(i32,@intCast(80)))))) continue; _tmp20.append(s.ss_ext_discount_amt) catch unreachable; } const _tmp21 = _tmp20.toOwnedSlice() catch unreachable; break :blk17 _tmp21; })) else (_avg_int(blk18: { var _tmp22 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(61))) and (s.ss_quantity <= @as(i32,@intCast(80)))))) continue; _tmp22.append(s.ss_net_paid) catch unreachable; } const _tmp23 = _tmp22.toOwnedSlice() catch unreachable; break :blk18 _tmp23; }));
    bucket5 = if (((blk19: { var _tmp24 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(81))) and (s.ss_quantity <= @as(i32,@intCast(100)))))) continue; _tmp24.append(s) catch unreachable; } const _tmp25 = _tmp24.toOwnedSlice() catch unreachable; break :blk19 _tmp25; }).len > @as(i32,@intCast(50)))) (_avg_int(blk20: { var _tmp26 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(81))) and (s.ss_quantity <= @as(i32,@intCast(100)))))) continue; _tmp26.append(s.ss_ext_discount_amt) catch unreachable; } const _tmp27 = _tmp26.toOwnedSlice() catch unreachable; break :blk20 _tmp27; })) else (_avg_int(blk21: { var _tmp28 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |s| { if (!(((s.ss_quantity >= @as(i32,@intCast(81))) and (s.ss_quantity <= @as(i32,@intCast(100)))))) continue; _tmp28.append(s.ss_net_paid) catch unreachable; } const _tmp29 = _tmp28.toOwnedSlice() catch unreachable; break :blk21 _tmp29; }));
    result = blk23: { var _tmp30 = std.ArrayList(std.AutoHashMap([]const u8, f64)).init(std.heap.page_allocator); for (reason) |r| { if (!((r.r_reason_sk == @as(i32,@intCast(1))))) continue; _tmp30.append(blk22: { var m = std.AutoHashMap(f64, f64).init(std.heap.page_allocator); m.put(bucket1, bucket1) catch unreachable; m.put(bucket2, bucket2) catch unreachable; m.put(bucket3, bucket3) catch unreachable; m.put(bucket4, bucket4) catch unreachable; m.put(bucket5, bucket5) catch unreachable; break :blk22 m; }) catch unreachable; } const _tmp31 = _tmp30.toOwnedSlice() catch unreachable; break :blk23 _tmp31; };
    _json(result);
    test_TPCDS_Q9_result();
}
