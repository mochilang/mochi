const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
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

const CallCenter = struct {
    cc_call_center_sk: i32,
    cc_call_center_id: []const u8,
    cc_name: []const u8,
    cc_manager: []const u8,
};

const CatalogReturn = struct {
    cr_call_center_sk: i32,
    cr_returned_date_sk: i32,
    cr_returning_customer_sk: i32,
    cr_net_loss: f64,
};

const DateDim = struct {
    d_date_sk: i32,
    d_year: i32,
    d_moy: i32,
};

const Customer = struct {
    c_customer_sk: i32,
    c_current_cdemo_sk: i32,
    c_current_hdemo_sk: i32,
    c_current_addr_sk: i32,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_gmt_offset: i32,
};

const CustomerDemographics = struct {
    cd_demo_sk: i32,
    cd_marital_status: []const u8,
    cd_education_status: []const u8,
};

const HouseholdDemographics = struct {
    hd_demo_sk: i32,
    hd_buy_potential: []const u8,
};

var call_center: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var household_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q91_returns() void {
    expect((result == blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("Call_Center", "CC1") catch unreachable; m.put("Call_Center_Name", "Main") catch unreachable; m.put("Manager", "Alice") catch unreachable; m.put("Returns_Loss", 10) catch unreachable; break :blk0 m; }));
}

pub fn main() void {
    call_center = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cc_call_center_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cc_call_center_id", "CC1") catch unreachable; m.put("cc_name", "Main") catch unreachable; m.put("cc_manager", "Alice") catch unreachable; break :blk1 m; }};
    catalog_returns = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cr_call_center_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cr_returned_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cr_returning_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cr_net_loss", 10) catch unreachable; break :blk2 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2001))) catch unreachable; m.put("d_moy", @as(i32,@intCast(5))) catch unreachable; break :blk3 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_hdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk4 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_marital_status", "M") catch unreachable; m.put("cd_education_status", "Unknown") catch unreachable; break :blk5 m; }};
    household_demographics = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("hd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("hd_buy_potential", "1001-5000") catch unreachable; break :blk6 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_gmt_offset", -@as(i32,@intCast(6))) catch unreachable; break :blk7 m; }};
    result = first(blk11: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (call_center) |cc| { for (catalog_returns) |cr| { if (!((cc.cc_call_center_sk == cr.cr_call_center_sk))) continue; for (date_dim) |d| { if (!((cr.cr_returned_date_sk == d.d_date_sk))) continue; for (customer) |c| { if (!((cr.cr_returning_customer_sk == c.c_customer_sk))) continue; for (customer_demographics) |cd| { if (!((c.c_current_cdemo_sk == cd.cd_demo_sk))) continue; for (household_demographics) |hd| { if (!((c.c_current_hdemo_sk == hd.hd_demo_sk))) continue; for (customer_address) |ca| { if (!((c.c_current_addr_sk == ca.ca_address_sk))) continue; if (!(((((((d.d_year == @as(i32,@intCast(2001))) and (d.d_moy == @as(i32,@intCast(5)))) and std.mem.eql(u8, cd.cd_marital_status, "M")) and std.mem.eql(u8, cd.cd_education_status, "Unknown")) and std.mem.eql(u8, hd.hd_buy_potential, "1001-5000")) and (ca.ca_gmt_offset == (-@as(i32,@intCast(6))))))) continue; const _tmp4 = blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", cc.cc_call_center_id) catch unreachable; m.put("name", cc.cc_name) catch unreachable; m.put("mgr", cc.cc_manager) catch unreachable; break :blk8 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(cc) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(cc) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("Call_Center", g.key.id) catch unreachable; m.put("Call_Center_Name", g.key.name) catch unreachable; m.put("Manager", g.key.mgr) catch unreachable; m.put("Returns_Loss", _sum_int(blk10: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.cr_net_loss) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk10 _tmp1; })) catch unreachable; break :blk9 m; }) catch unreachable; } break :blk11 _tmp5.toOwnedSlice() catch unreachable; });
    _json(result);
    test_TPCDS_Q91_returns();
}
