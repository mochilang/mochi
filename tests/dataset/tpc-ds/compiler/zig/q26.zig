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
    cs_sold_date_sk: i32,
    cs_item_sk: i32,
    cs_bill_cdemo_sk: i32,
    cs_promo_sk: i32,
    cs_quantity: i32,
    cs_list_price: f64,
    cs_coupon_amt: f64,
    cs_sales_price: f64,
};

const CustomerDemo = struct {
    cd_demo_sk: i32,
    cd_gender: []const u8,
    cd_marital_status: []const u8,
    cd_education_status: []const u8,
};

const DateDim = struct {
    d_date_sk: i32,
    d_year: i32,
};

const Item = struct {
    i_item_sk: i32,
    i_item_id: []const u8,
};

const Promotion = struct {
    p_promo_sk: i32,
    p_channel_email: []const u8,
    p_channel_event: []const u8,
};

var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_demographics: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var promotion: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q26_demographic_averages() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("i_item_id", "ITEM1") catch unreachable; m.put("agg1", 10) catch unreachable; m.put("agg2", 100) catch unreachable; m.put("agg3", 5) catch unreachable; m.put("agg4", 95) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_bill_cdemo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_promo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(10))) catch unreachable; m.put("cs_list_price", 100) catch unreachable; m.put("cs_coupon_amt", 5) catch unreachable; m.put("cs_sales_price", 95) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_bill_cdemo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_promo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cs_quantity", @as(i32,@intCast(5))) catch unreachable; m.put("cs_list_price", 50) catch unreachable; m.put("cs_coupon_amt", 2) catch unreachable; m.put("cs_sales_price", 48) catch unreachable; break :blk2 m; }};
    customer_demographics = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cd_gender", "M") catch unreachable; m.put("cd_marital_status", "S") catch unreachable; m.put("cd_education_status", "College") catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cd_demo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("cd_gender", "F") catch unreachable; m.put("cd_marital_status", "M") catch unreachable; m.put("cd_education_status", "High School") catch unreachable; break :blk4 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk5 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_item_id", "ITEM1") catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_item_id", "ITEM2") catch unreachable; break :blk7 m; }};
    promotion = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("p_promo_sk", @as(i32,@intCast(1))) catch unreachable; m.put("p_channel_email", "N") catch unreachable; m.put("p_channel_event", "Y") catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("p_promo_sk", @as(i32,@intCast(2))) catch unreachable; m.put("p_channel_email", "Y") catch unreachable; m.put("p_channel_event", "N") catch unreachable; break :blk9 m; }};
    result = blk15: { var _tmp8 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp9 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (customer_demographics) |cd| { if (!((cs.cs_bill_cdemo_sk == cd.cd_demo_sk))) continue; for (date_dim) |d| { if (!((cs.cs_sold_date_sk == d.d_date_sk))) continue; for (item) |i| { if (!((cs.cs_item_sk == i.i_item_sk))) continue; for (promotion) |p| { if (!((cs.cs_promo_sk == p.p_promo_sk))) continue; if (!(((((std.mem.eql(u8, cd.cd_gender, "M") and std.mem.eql(u8, cd.cd_marital_status, "S")) and std.mem.eql(u8, cd.cd_education_status, "College")) and ((std.mem.eql(u8, p.p_channel_email, "N") or std.mem.eql(u8, p.p_channel_event, "N")))) and (d.d_year == @as(i32,@intCast(2000)))))) continue; const _tmp10 = i.i_item_id; if (_tmp9.get(_tmp10)) |idx| { _tmp8.items[idx].Items.append(cs) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp10, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(cs) catch unreachable; _tmp8.append(g) catch unreachable; _tmp9.put(_tmp10, _tmp8.items.len - 1) catch unreachable; } } } } } } var _tmp11 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp11.append(blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_id", g.key) catch unreachable; m.put("agg1", _avg_int(blk11: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.cs_quantity) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk11 _tmp1; })) catch unreachable; m.put("agg2", _avg_int(blk12: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp2.append(x.cs_list_price) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk12 _tmp3; })) catch unreachable; m.put("agg3", _avg_int(blk13: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp4.append(x.cs_coupon_amt) catch unreachable; } const _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk13 _tmp5; })) catch unreachable; m.put("agg4", _avg_int(blk14: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp6.append(x.cs_sales_price) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk14 _tmp7; })) catch unreachable; break :blk10 m; }) catch unreachable; } break :blk15 _tmp11.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q26_demographic_averages();
}
