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

var web_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_total_return: []const std.AutoHashMap([]const u8, i32) = undefined;
var avg_by_state: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q30_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("c_customer_id", "C1") catch unreachable; m.put("c_first_name", "John") catch unreachable; m.put("c_last_name", "Doe") catch unreachable; m.put("ctr_total_return", 150) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    web_returns = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("wr_returning_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("wr_returned_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("wr_return_amt", 100) catch unreachable; m.put("wr_returning_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("wr_returning_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("wr_returned_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("wr_return_amt", 30) catch unreachable; m.put("wr_returning_addr_sk", @as(i32,@intCast(2))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("wr_returning_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("wr_returned_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("wr_return_amt", 50) catch unreachable; m.put("wr_returning_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk4 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_state", "CA") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ca_state", "CA") catch unreachable; break :blk6 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_customer_id", "C1") catch unreachable; m.put("c_first_name", "John") catch unreachable; m.put("c_last_name", "Doe") catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_customer_id", "C2") catch unreachable; m.put("c_first_name", "Jane") catch unreachable; m.put("c_last_name", "Smith") catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(2))) catch unreachable; break :blk8 m; }};
    customer_total_return = blk12: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (web_returns) |wr| { for (date_dim) |d| { if (!((wr.wr_returned_date_sk == d.d_date_sk))) continue; for (customer_address) |ca| { if (!((wr.wr_returning_addr_sk == ca.ca_address_sk))) continue; if (!(((d.d_year == @as(i32,@intCast(2000))) and std.mem.eql(u8, ca.ca_state, "CA")))) continue; const _tmp4 = blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cust", wr.wr_returning_customer_sk) catch unreachable; m.put("state", ca.ca_state) catch unreachable; break :blk9 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(wr) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(wr) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ctr_customer_sk", g.key.cust) catch unreachable; m.put("ctr_state", g.key.state) catch unreachable; m.put("ctr_total_return", _sum_int(blk11: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.wr_return_amt) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk11 _tmp1; })) catch unreachable; break :blk10 m; }) catch unreachable; } break :blk12 _tmp5.toOwnedSlice() catch unreachable; };
    avg_by_state = blk15: { var _tmp8 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (customer_total_return) |ctr| { const _tmp10 = ctr.ctr_state; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(ctr) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ctr) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("state", g.key) catch unreachable; m.put("avg_return", _avg_int(blk14: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.ctr_total_return) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk14 _tmp7; })) catch unreachable; break :blk13 m; }) catch unreachable; } break :blk15 _tmp11.toOwnedSlice() catch unreachable; };
    result = blk17: { var _tmp12 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (customer_total_return) |ctr| { for (avg_by_state) |avg| { if (!((ctr.ctr_state == avg.state))) continue; for (customer) |c| { if (!((ctr.ctr_customer_sk == c.c_customer_sk))) continue; if (!((ctr.ctr_total_return > (avg.avg_return * 1.2)))) continue; _tmp12.append(blk16: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_id", c.c_customer_id) catch unreachable; m.put("c_first_name", c.c_first_name) catch unreachable; m.put("c_last_name", c.c_last_name) catch unreachable; m.put("ctr_total_return", ctr.ctr_total_return) catch unreachable; break :blk16 m; }) catch unreachable; } } } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk17 _tmp13; };
    _json(result);
    test_TPCDS_Q30_simplified();
}
