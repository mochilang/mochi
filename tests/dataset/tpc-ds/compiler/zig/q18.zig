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

const CatalogSale = struct {
    cs_quantity: i32,
    cs_list_price: f64,
    cs_coupon_amt: f64,
    cs_sales_price: f64,
    cs_net_profit: f64,
    cs_bill_cdemo_sk: i32,
    cs_bill_customer_sk: i32,
    cs_sold_date_sk: i32,
    cs_item_sk: i32,
};

const CustomerDemographics = struct {
    cd_demo_sk: i32,
    cd_gender: []const u8,
    cd_education_status: []const u8,
    cd_dep_count: i32,
};

const Customer = struct {
    c_customer_sk: i32,
    c_current_cdemo_sk: i32,
    c_current_addr_sk: i32,
    c_birth_year: i32,
    c_birth_month: i32,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_country: []const u8,
    ca_state: []const u8,
    ca_county: []const u8,
};

const DateDim = struct {
    d_date_sk: i32,
    d_year: i32,
};

const Item = struct {
    i_item_sk: i32,
    i_item_id: []const u8,
};

var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var joined: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q18_averages() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "I1") catch unreachable; m.put("ca_country", "US") catch unreachable; m.put("ca_state", "CA") catch unreachable; m.put("ca_county", "County1") catch unreachable; m.put("agg1", 1) catch unreachable; m.put("agg2", 10) catch unreachable; m.put("agg3", 1) catch unreachable; m.put("agg4", 9) catch unreachable; m.put("agg5", 2) catch unreachable; m.put("agg6", 1980) catch unreachable; m.put("agg7", 2) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_quantity", @as(i32,@intCast(1))) catch unreachable; m.put("cs_list_price", 10) catch unreachable; m.put("cs_coupon_amt", 1) catch unreachable; m.put("cs_sales_price", 9) catch unreachable; m.put("cs_net_profit", 2) catch unreachable; m.put("cs_bill_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_gender", "M") catch unreachable; m.put("cd_education_status", "College") catch unreachable; m.put("cd_dep_count", @as(i32,@intCast(2))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cd_gender", "F") catch unreachable; m.put("cd_education_status", "College") catch unreachable; m.put("cd_dep_count", @as(i32,@intCast(2))) catch unreachable; break :blk3 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_cdemo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_birth_year", @as(i32,@intCast(1980))) catch unreachable; m.put("c_birth_month", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_country", "US") catch unreachable; m.put("ca_state", "CA") catch unreachable; m.put("ca_county", "County1") catch unreachable; break :blk5 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(1999))) catch unreachable; break :blk6 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "I1") catch unreachable; break :blk7 m; }};
    joined = blk9: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (customer_demographics) |cd1| { if (!((((cs.cs_bill_cdemo_sk == cd1.cd_demo_sk) and std.mem.eql(u8, cd1.cd_gender, "M")) and std.mem.eql(u8, cd1.cd_education_status, "College")))) continue; for (customer) |c| { if (!((cs.cs_bill_customer_sk == c.c_customer_sk))) continue; for (customer_demographics) |cd2| { if (!((c.c_current_cdemo_sk == cd2.cd_demo_sk))) continue; for (customer_address) |ca| { if (!((c.c_current_addr_sk == ca.ca_address_sk))) continue; for (date_dim) |d| { if (!(((cs.cs_sold_date_sk == d.d_date_sk) and (d.d_year == @as(i32,@intCast(1999)))))) continue; for (item) |i| { if (!((cs.cs_item_sk == i.i_item_sk))) continue; _tmp0.append(blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", i.i_item_id) catch unreachable; m.put("ca_country", ca.ca_country) catch unreachable; m.put("ca_state", ca.ca_state) catch unreachable; m.put("ca_county", ca.ca_county) catch unreachable; m.put("q", cs.cs_quantity) catch unreachable; m.put("lp", cs.cs_list_price) catch unreachable; m.put("cp", cs.cs_coupon_amt) catch unreachable; m.put("sp", cs.cs_sales_price) catch unreachable; m.put("np", cs.cs_net_profit) catch unreachable; m.put("by", c.c_birth_year) catch unreachable; m.put("dep", cd1.cd_dep_count) catch unreachable; break :blk8 m; }) catch unreachable; } } } } } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk9 _tmp1; };
    result = blk19: { var _tmp16 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp17 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (joined) |j| { const _tmp18 = blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", j.i_item_id) catch unreachable; m.put("ca_country", j.ca_country) catch unreachable; m.put("ca_state", j.ca_state) catch unreachable; m.put("ca_county", j.ca_county) catch unreachable; break :blk10 m; }; if (_tmp17.get(_tmp18)) |idx| { _tmp16.items[idx].Items.append(j) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp18, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(j) catch unreachable; _tmp16.append(g) catch unreachable; _tmp17.put(_tmp18, _tmp16.items.len - 1) catch unreachable; } } var _tmp19 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp16.items) |g| { _tmp19.append(blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.key.i_item_id) catch unreachable; m.put("ca_country", g.key.ca_country) catch unreachable; m.put("ca_state", g.key.ca_state) catch unreachable; m.put("ca_county", g.key.ca_county) catch unreachable; m.put("agg1", _avg_int(blk12: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.q) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk12 _tmp3; })) catch unreachable; m.put("agg2", _avg_int(blk13: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(x.lp) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk13 _tmp5; })) catch unreachable; m.put("agg3", _avg_int(blk14: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.cp) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk14 _tmp7; })) catch unreachable; m.put("agg4", _avg_int(blk15: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp8.append(x.sp) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk15 _tmp9; })) catch unreachable; m.put("agg5", _avg_int(blk16: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp10.append(x.np) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk16 _tmp11; })) catch unreachable; m.put("agg6", _avg_int(blk17: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp12.append(x.by) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk17 _tmp13; })) catch unreachable; m.put("agg7", _avg_int(blk18: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp14.append(x.dep) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk18 _tmp15; })) catch unreachable; break :blk11 m; }) catch unreachable; } break :blk19 _tmp19.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q18_averages();
}
