const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
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
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var groups: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q73_simplified() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("c_last_name", "Smith") catch unreachable; m.put("c_first_name", "Alice") catch unreachable; m.put("c_salutation", "Ms.") catch unreachable; m.put("c_preferred_cust_flag", "Y") catch unreachable; m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("cnt", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_dom", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1998))) catch unreachable; break :blk2 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_county", "A") catch unreachable; break :blk3 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hd_buy_potential", "1001-5000") catch unreachable; m.put("hd_vehicle_count", @as(i32,@intCast(2))) catch unreachable; m.put("hd_dep_count", @as(i32,@intCast(3))) catch unreachable; break :blk4 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_last_name", "Smith") catch unreachable; m.put("c_first_name", "Alice") catch unreachable; m.put("c_salutation", "Ms.") catch unreachable; m.put("c_preferred_cust_flag", "Y") catch unreachable; break :blk5 m; }};
    groups = blk8: { var _tmp0 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp1 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((d.d_date_sk == ss.ss_sold_date_sk))) continue; for (store) |s| { if (!((s.s_store_sk == ss.ss_store_sk))) continue; for (household_demographics) |hd| { if (!((hd.hd_demo_sk == ss.ss_hdemo_sk))) continue; if (!((((((((d.d_dom >= @as(i32,@intCast(1))) and (d.d_dom <= @as(i32,@intCast(2)))) and ((std.mem.eql(u8, hd.hd_buy_potential, "1001-5000") or std.mem.eql(u8, hd.hd_buy_potential, "0-500")))) and (hd.hd_vehicle_count > @as(i32,@intCast(0)))) and ((hd.hd_dep_count / hd.hd_vehicle_count) > @as(i32,@intCast(1)))) and ((((d.d_year == @as(i32,@intCast(1998))) or (d.d_year == @as(i32,@intCast(1999)))) or (d.d_year == @as(i32,@intCast(2000)))))) and std.mem.eql(u8, s.s_county, "A")))) continue; const _tmp2 = blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ticket", ss.ss_ticket_number) catch unreachable; m.put("cust", ss.ss_customer_sk) catch unreachable; break :blk6 m; }; if (_tmp1.get(_tmp2)) |idx| { _tmp0.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp2, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp0.append(g) catch unreachable; _tmp1.put(_tmp2, _tmp0.items.len - 1) catch unreachable; } } } } } var _tmp3 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp0.items) |g| { _tmp3.append(blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("key", g.key) catch unreachable; m.put("cnt", (g.Items.len)) catch unreachable; break :blk7 m; }) catch unreachable; } break :blk8 _tmp3.toOwnedSlice() catch unreachable; };
    result = blk10: { var _tmp4 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (groups) |g| { for (customer) |c| { if (!((c.c_customer_sk == g.key.cust))) continue; if (!(((g.cnt >= @as(i32,@intCast(1))) and (g.cnt <= @as(i32,@intCast(5)))))) continue; _tmp4.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_last_name", c.c_last_name) catch unreachable; m.put("c_first_name", c.c_first_name) catch unreachable; m.put("c_salutation", c.c_salutation) catch unreachable; m.put("c_preferred_cust_flag", c.c_preferred_cust_flag) catch unreachable; m.put("ss_ticket_number", g.key.ticket) catch unreachable; m.put("cnt", g.cnt) catch unreachable; break :blk9 m; }) catch unreachable; } } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk10 _tmp5; };
    _json(result);
    test_TPCDS_Q73_simplified();
}
