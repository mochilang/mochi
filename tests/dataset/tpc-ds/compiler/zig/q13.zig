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

const StoreSale = struct {
    ss_store_sk: i32,
    ss_sold_date_sk: i32,
    ss_hdemo_sk: i32,
    ss_cdemo_sk: i32,
    ss_addr_sk: i32,
    ss_sales_price: f64,
    ss_net_profit: f64,
    ss_quantity: i32,
    ss_ext_sales_price: f64,
    ss_ext_wholesale_cost: f64,
};

const Store = struct {
    s_store_sk: i32,
    s_state: []const u8,
};

const CustomerDemographics = struct {
    cd_demo_sk: i32,
    cd_marital_status: []const u8,
    cd_education_status: []const u8,
};

const HouseholdDemographics = struct {
    hd_demo_sk: i32,
    hd_dep_count: i32,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_country: []const u8,
    ca_state: []const u8,
};

const DateDim = struct {
    d_date_sk: i32,
    d_year: i32,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var filtered: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, f64) = undefined;

fn test_TPCDS_Q13_averages() void {
    expect((result == &[_]std.AutoHashMap([]const u8, f64){blk0: { var m = std.AutoHashMap(i32, f64).init(std.heap.page_allocator); m.put("avg_ss_quantity", 10) catch unreachable; m.put("avg_ss_ext_sales_price", 100) catch unreachable; m.put("avg_ss_ext_wholesale_cost", 50) catch unreachable; m.put("sum_ss_ext_wholesale_cost", 50) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sales_price", 120) catch unreachable; m.put("ss_net_profit", 150) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(10))) catch unreachable; m.put("ss_ext_sales_price", 100) catch unreachable; m.put("ss_ext_wholesale_cost", 50) catch unreachable; break :blk1 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_state", "CA") catch unreachable; break :blk2 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_marital_status", "M1") catch unreachable; m.put("cd_education_status", "ES1") catch unreachable; break :blk3 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hd_dep_count", @as(i32,@intCast(3))) catch unreachable; break :blk4 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_country", "United States") catch unreachable; m.put("ca_state", "CA") catch unreachable; break :blk5 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2001))) catch unreachable; break :blk6 m; }};
    filtered = blk7: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (store) |s| { if (!((ss.ss_store_sk == s.s_store_sk))) continue; for (customer_demographics) |cd| { if (!((((ss.ss_cdemo_sk == cd.cd_demo_sk) and std.mem.eql(u8, cd.cd_marital_status, "M1")) and std.mem.eql(u8, cd.cd_education_status, "ES1")))) continue; for (household_demographics) |hd| { if (!(((ss.ss_hdemo_sk == hd.hd_demo_sk) and (hd.hd_dep_count == @as(i32,@intCast(3)))))) continue; for (customer_address) |ca| { if (!((((ss.ss_addr_sk == ca.ca_address_sk) and std.mem.eql(u8, ca.ca_country, "United States")) and std.mem.eql(u8, ca.ca_state, "CA")))) continue; for (date_dim) |d| { if (!(((ss.ss_sold_date_sk == d.d_date_sk) and (d.d_year == @as(i32,@intCast(2001)))))) continue; _tmp0.append(ss) catch unreachable; } } } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; };
    result = blk14: { var _tmp10 = std.ArrayList(struct { key: std.AutoHashMap(i32, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp11 = std.AutoHashMap(std.AutoHashMap(i32, i32), usize).init(std.heap.page_allocator); for (filtered) |r| { const _tmp12 = blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); break :blk8 m; }; if (_tmp11.get(_tmp12)) |idx| { _tmp10.items[idx].Items.append(r) catch unreachable; } else { var g = struct { key: std.AutoHashMap(i32, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp12, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(r) catch unreachable; _tmp10.append(g) catch unreachable; _tmp11.put(_tmp12, _tmp10.items.len - 1) catch unreachable; } } var _tmp13 = std.ArrayList(std.AutoHashMap([]const u8, f64)).init(std.heap.page_allocator);for (_tmp10.items) |g| { _tmp13.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("avg_ss_quantity", _avg_int(blk10: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.ss_quantity) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk10 _tmp3; })) catch unreachable; m.put("avg_ss_ext_sales_price", _avg_int(blk11: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(x.ss_ext_sales_price) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk11 _tmp5; })) catch unreachable; m.put("avg_ss_ext_wholesale_cost", _avg_int(blk12: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.ss_ext_wholesale_cost) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk12 _tmp7; })) catch unreachable; m.put("sum_ss_ext_wholesale_cost", _sum_int(blk13: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp8.append(x.ss_ext_wholesale_cost) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk13 _tmp9; })) catch unreachable; break :blk9 m; }) catch unreachable; } break :blk14 _tmp13.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q13_averages();
}
