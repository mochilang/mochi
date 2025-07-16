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

const StoreSale = struct {
    ss_customer_sk: i32,
    ss_item_sk: i32,
};

const CatalogSale = struct {
    cs_bill_customer_sk: i32,
    cs_item_sk: i32,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var ssci: []const std.AutoHashMap([]const u8, i32) = undefined;
var csci: []const std.AutoHashMap([]const u8, i32) = undefined;
var joined: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: std.AutoHashMap([]const u8, f64) = undefined;

fn test_TPCDS_Q97_overlap() void {
    expect((((result.store_only == @as(i32,@intCast(1))) and (result.catalog_only == @as(i32,@intCast(1)))) and (result.store_and_catalog == @as(i32,@intCast(1)))));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_bill_customer_sk", @as(i32,@intCast(3))) catch unreachable; m.put("cs_item_sk", @as(i32,@intCast(2))) catch unreachable; break :blk3 m; }};
    ssci = blk6: { var _tmp0 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp1 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { const _tmp2 = blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_sk", ss.ss_customer_sk) catch unreachable; m.put("item_sk", ss.ss_item_sk) catch unreachable; break :blk4 m; }; if (_tmp1.get(_tmp2)) |idx| { _tmp0.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp2, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp0.append(g) catch unreachable; _tmp1.put(_tmp2, _tmp0.items.len - 1) catch unreachable; } } var _tmp3 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp0.items) |g| { _tmp3.append(blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_sk", g.key.customer_sk) catch unreachable; m.put("item_sk", g.key.item_sk) catch unreachable; break :blk5 m; }) catch unreachable; } break :blk6 _tmp3.toOwnedSlice() catch unreachable; };
    csci = blk9: { var _tmp4 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (catalog_sales) |cs| { const _tmp6 = blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_sk", cs.cs_bill_customer_sk) catch unreachable; m.put("item_sk", cs.cs_item_sk) catch unreachable; break :blk7 m; }; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(cs) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(cs) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("customer_sk", g.key.customer_sk) catch unreachable; m.put("item_sk", g.key.item_sk) catch unreachable; break :blk8 m; }) catch unreachable; } break :blk9 _tmp7.toOwnedSlice() catch unreachable; };
    joined = blk11: { var _tmp8 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (ssci) |s| { for (csci) |c| { if (!(((s.customer_sk == c.customer_sk) and (s.item_sk == c.item_sk)))) continue; _tmp8.append(blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("store_only", if (((s.customer_sk != 0) and (c.customer_sk == 0))) (@as(i32,@intCast(1))) else (@as(i32,@intCast(0)))) catch unreachable; m.put("catalog_only", if (((s.customer_sk == 0) and (c.customer_sk != 0))) (@as(i32,@intCast(1))) else (@as(i32,@intCast(0)))) catch unreachable; m.put("both", if (((s.customer_sk != 0) and (c.customer_sk != 0))) (@as(i32,@intCast(1))) else (@as(i32,@intCast(0)))) catch unreachable; break :blk10 m; }) catch unreachable; } } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk11 _tmp9; };
    result = blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("store_only", _sum_int(blk13: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (joined) |x| { _tmp10.append(x.store_only) catch unreachable; } const _tmp11 = _tmp10.toOwnedSlice() catch unreachable; break :blk13 _tmp11; })) catch unreachable; m.put("catalog_only", _sum_int(blk14: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); for (joined) |x| { _tmp12.append(x.catalog_only) catch unreachable; } const _tmp13 = _tmp12.toOwnedSlice() catch unreachable; break :blk14 _tmp13; })) catch unreachable; m.put("store_and_catalog", _sum_int(blk15: { var _tmp14 = std.ArrayList(i32).init(std.heap.page_allocator); for (joined) |x| { _tmp14.append(x.both) catch unreachable; } const _tmp15 = _tmp14.toOwnedSlice() catch unreachable; break :blk15 _tmp15; })) catch unreachable; break :blk12 m; };
    _json(result);
    test_TPCDS_Q97_overlap();
}
