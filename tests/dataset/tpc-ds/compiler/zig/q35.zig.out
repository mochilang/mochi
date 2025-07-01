const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _contains_list_int(v: []const i32, item: i32) bool {
    for (v) |it| { if (it == item) return true; }
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

var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var purchased: []const i32 = undefined;
var groups: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q35_simplified() void {
    expect((groups == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_state", "CA") catch unreachable; m.put("cd_gender", "M") catch unreachable; m.put("cd_marital_status", "S") catch unreachable; m.put("cd_dep_count", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_employed_count", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_college_count", @as(i32,@intCast(0))) catch unreachable; m.put("cnt", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    customer = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_current_cdemo_sk", @as(i32,@intCast(2))) catch unreachable; break :blk2 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_state", "CA") catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ca_state", "NY") catch unreachable; break :blk4 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_gender", "M") catch unreachable; m.put("cd_marital_status", "S") catch unreachable; m.put("cd_dep_count", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_employed_count", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_college_count", @as(i32,@intCast(0))) catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cd_gender", "F") catch unreachable; m.put("cd_marital_status", "M") catch unreachable; m.put("cd_dep_count", @as(i32,@intCast(2))) catch unreachable; m.put("cd_dep_employed_count", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_college_count", @as(i32,@intCast(1))) catch unreachable; break :blk6 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk7 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; break :blk8 m; }};
    purchased = blk9: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; if (!(((d.d_year == @as(i32,@intCast(2000))) and (d.d_qoy < @as(i32,@intCast(4)))))) continue; _tmp0.append(ss.ss_customer_sk) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk9 _tmp1; };
    groups = blk12: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (customer) |c| { for (customer_address) |ca| { if (!((c.c_current_addr_sk == ca.ca_address_sk))) continue; for (customer_demographics) |cd| { if (!((c.c_current_cdemo_sk == cd.cd_demo_sk))) continue; if (!(_contains_list_int(purchased, c.c_customer_sk))) continue; const _tmp4 = blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("state", ca.ca_state) catch unreachable; m.put("gender", cd.cd_gender) catch unreachable; m.put("marital", cd.cd_marital_status) catch unreachable; m.put("dep", cd.cd_dep_count) catch unreachable; m.put("emp", cd.cd_dep_employed_count) catch unreachable; m.put("col", cd.cd_dep_college_count) catch unreachable; break :blk10 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_state", g.key.state) catch unreachable; m.put("cd_gender", g.key.gender) catch unreachable; m.put("cd_marital_status", g.key.marital) catch unreachable; m.put("cd_dep_count", g.key.dep) catch unreachable; m.put("cd_dep_employed_count", g.key.emp) catch unreachable; m.put("cd_dep_college_count", g.key.col) catch unreachable; m.put("cnt", (g.Items.len)) catch unreachable; break :blk11 m; }) catch unreachable; } break :blk12 _tmp5.toOwnedSlice() catch unreachable; };
    _json(groups);
    test_TPCDS_Q35_simplified();
}
