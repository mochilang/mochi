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

var store_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_total_return: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q1_result() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("c_customer_id", "C2") catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_returns = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_returned_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_store_sk", @as(i32,@intCast(10))) catch unreachable; m.put("sr_return_amt", 20) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_returned_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("sr_store_sk", @as(i32,@intCast(10))) catch unreachable; m.put("sr_return_amt", 50) catch unreachable; break :blk2 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; break :blk3 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(10))) catch unreachable; m.put("s_state", "TN") catch unreachable; break :blk4 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_customer_id", "C1") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_customer_id", "C2") catch unreachable; break :blk6 m; }};
    customer_total_return = blk10: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_returns) |sr| { for (date_dim) |d| { if (!(((sr.sr_returned_date_sk == d.d_date_sk) and (d.d_year == @as(i32,@intCast(1998)))))) continue; const _tmp4 = blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_sk", sr.sr_customer_sk) catch unreachable; m.put("store_sk", sr.sr_store_sk) catch unreachable; break :blk7 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(sr) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(sr) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ctr_customer_sk", g.key.customer_sk) catch unreachable; m.put("ctr_store_sk", g.key.store_sk) catch unreachable; m.put("ctr_total_return", _sum_int(blk9: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.sr_return_amt) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk9 _tmp1; })) catch unreachable; break :blk8 m; }) catch unreachable; } break :blk10 _tmp5.toOwnedSlice() catch unreachable; };
    result = blk13: { var _tmp8 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (customer_total_return) |ctr1| { for (store) |s| { if (!((ctr1.ctr_store_sk == s.s_store_sk))) continue; for (customer) |c| { if (!((ctr1.ctr_customer_sk == c.c_customer_sk))) continue; if (!(((ctr1.ctr_total_return > (_avg_int(blk12: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (customer_total_return) |ctr2| { if (!((ctr1.ctr_store_sk == ctr2.ctr_store_sk))) continue; _tmp6.append(ctr2.ctr_total_return) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk12 _tmp7; }) * 1.2)) and std.mem.eql(u8, s.s_state, "TN")))) continue; _tmp8.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_id", c.c_customer_id) catch unreachable; break :blk11 m; }) catch unreachable; } } } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk13 _tmp9; };
    _json(result);
    test_TPCDS_Q1_result();
}
