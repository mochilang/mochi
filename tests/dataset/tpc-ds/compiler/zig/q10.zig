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

const Customer = struct {
    c_customer_sk: i32,
    c_current_addr_sk: i32,
    c_current_cdemo_sk: i32,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_county: []const u8,
};

const CustomerDemographics = struct {
    cd_demo_sk: i32,
    cd_gender: []const u8,
    cd_marital_status: []const u8,
    cd_education_status: []const u8,
    cd_purchase_estimate: i32,
    cd_credit_rating: []const u8,
    cd_dep_count: i32,
    cd_dep_employed_count: i32,
    cd_dep_college_count: i32,
};

const StoreSale = struct {
    ss_customer_sk: i32,
    ss_sold_date_sk: i32,
};

const DateDim = struct {
    d_date_sk: i32,
    d_year: i32,
    d_moy: i32,
};

var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var web_sales: []const i32 = undefined;
var catalog_sales: []const i32 = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var active: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q10_demographics_count() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cd_gender", "F") catch unreachable; m.put("cd_marital_status", "M") catch unreachable; m.put("cd_education_status", "College") catch unreachable; m.put("cnt1", @as(i32,@intCast(1))) catch unreachable; m.put("cd_purchase_estimate", @as(i32,@intCast(5000))) catch unreachable; m.put("cnt2", @as(i32,@intCast(1))) catch unreachable; m.put("cd_credit_rating", "Good") catch unreachable; m.put("cnt3", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_count", @as(i32,@intCast(1))) catch unreachable; m.put("cnt4", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_employed_count", @as(i32,@intCast(1))) catch unreachable; m.put("cnt5", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_college_count", @as(i32,@intCast(0))) catch unreachable; m.put("cnt6", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    customer = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_county", "CountyA") catch unreachable; break :blk2 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_gender", "F") catch unreachable; m.put("cd_marital_status", "M") catch unreachable; m.put("cd_education_status", "College") catch unreachable; m.put("cd_purchase_estimate", @as(i32,@intCast(5000))) catch unreachable; m.put("cd_credit_rating", "Good") catch unreachable; m.put("cd_dep_count", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_employed_count", @as(i32,@intCast(1))) catch unreachable; m.put("cd_dep_college_count", @as(i32,@intCast(0))) catch unreachable; break :blk3 m; }};
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }};
    web_sales = &[_]i32{};
    catalog_sales = &[_]i32{};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; m.put("d_moy", @as(i32,@intCast(2))) catch unreachable; break :blk5 m; }};
    active = blk7: { var _tmp2 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (customer) |c| { for (customer_address) |ca| { if (!(((c.c_current_addr_sk == ca.ca_address_sk) and std.mem.eql(u8, ca.ca_county, "CountyA")))) continue; for (customer_demographics) |cd| { if (!((c.c_current_cdemo_sk == cd.cd_demo_sk))) continue; if (!(exists(blk6: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (date_dim) |d| { if (!((ss.ss_sold_date_sk == d.d_date_sk))) continue; if (!(((((ss.ss_customer_sk == c.c_customer_sk) and (d.d_year == @as(i32,@intCast(2000)))) and (d.d_moy >= @as(i32,@intCast(2)))) and (d.d_moy <= @as(i32,@intCast(5)))))) continue; _tmp0.append(ss) catch unreachable; } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk6 _tmp1; }))) continue; _tmp2.append(cd) catch unreachable; } } } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk7 _tmp3; };
    result = blk16: { var _tmp16 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp17 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (active) |a| { const _tmp18 = blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("gender", a.cd_gender) catch unreachable; m.put("marital", a.cd_marital_status) catch unreachable; m.put("education", a.cd_education_status) catch unreachable; m.put("purchase", a.cd_purchase_estimate) catch unreachable; m.put("credit", a.cd_credit_rating) catch unreachable; m.put("dep", a.cd_dep_count) catch unreachable; m.put("depemp", a.cd_dep_employed_count) catch unreachable; m.put("depcol", a.cd_dep_college_count) catch unreachable; break :blk8 m; }; if (_tmp17.get(_tmp18)) |idx| { _tmp16.items[idx].Items.append(a) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp18, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(a) catch unreachable; _tmp16.append(g) catch unreachable; _tmp17.put(_tmp18, _tmp16.items.len - 1) catch unreachable; } } var _tmp19 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp16.items) |g| { _tmp19.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_gender", g.key.gender) catch unreachable; m.put("cd_marital_status", g.key.marital) catch unreachable; m.put("cd_education_status", g.key.education) catch unreachable; m.put("cnt1", (blk10: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp4.append(_) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk10 _tmp5; }).len) catch unreachable; m.put("cd_purchase_estimate", g.key.purchase) catch unreachable; m.put("cnt2", (blk11: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp6.append(_) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk11 _tmp7; }).len) catch unreachable; m.put("cd_credit_rating", g.key.credit) catch unreachable; m.put("cnt3", (blk12: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp8.append(_) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk12 _tmp9; }).len) catch unreachable; m.put("cd_dep_count", g.key.dep) catch unreachable; m.put("cnt4", (blk13: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp10.append(_) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk13 _tmp11; }).len) catch unreachable; m.put("cd_dep_employed_count", g.key.depemp) catch unreachable; m.put("cnt5", (blk14: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp12.append(_) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk14 _tmp13; }).len) catch unreachable; m.put("cd_dep_college_count", g.key.depcol) catch unreachable; m.put("cnt6", (blk15: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |_| { _tmp14.append(_) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk15 _tmp15; }).len) catch unreachable; break :blk9 m; }) catch unreachable; } break :blk16 _tmp19.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q10_demographics_count();
}
