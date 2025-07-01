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
    ss_item_sk: i32,
    ss_ticket_number: i32,
    ss_customer_sk: i32,
    ss_quantity: i32,
    ss_sales_price: f64,
};

const StoreReturn = struct {
    sr_item_sk: i32,
    sr_ticket_number: i32,
    sr_reason_sk: i32,
    sr_return_quantity: i32,
};

const Reason = struct {
    r_reason_sk: i32,
    r_reason_desc: []const u8,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var reason: []const std.AutoHashMap([]const u8, i32) = undefined;
var t: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q93_active_sales() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sumsales", 40) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(5))) catch unreachable; m.put("ss_sales_price", 10) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_ticket_number", @as(i32,@intCast(2))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_quantity", @as(i32,@intCast(3))) catch unreachable; m.put("ss_sales_price", 20) catch unreachable; break :blk2 m; }};
    store_returns = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("sr_reason_sk", @as(i32,@intCast(1))) catch unreachable; m.put("sr_return_quantity", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }};
    reason = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("r_reason_sk", @as(i32,@intCast(1))) catch unreachable; m.put("r_reason_desc", "ReasonA") catch unreachable; break :blk4 m; }};
    t = blk6: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (store_sales) |ss| { for (store_returns) |sr| { if (!(((ss.ss_item_sk == sr.sr_item_sk) and (ss.ss_ticket_number == sr.sr_ticket_number)))) continue; for (reason) |r| { if (!((sr.sr_reason_sk == r.r_reason_sk))) continue; if (!(std.mem.eql(u8, r.r_reason_desc, "ReasonA"))) continue; _tmp0.append(blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", ss.ss_customer_sk) catch unreachable; m.put("act_sales", if ((sr != 0)) ((((ss.ss_quantity - sr.sr_return_quantity)) * ss.ss_sales_price)) else ((ss.ss_quantity * ss.ss_sales_price))) catch unreachable; break :blk5 m; }) catch unreachable; } } } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk6 _tmp1; };
    result = blk9: { var _tmp4 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (t) |x| { const _tmp6 = x.ss_customer_sk; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(x) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(x) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_customer_sk", g.key) catch unreachable; m.put("sumsales", _sum_int(blk8: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |y| { _tmp2.append(y.act_sales) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk8 _tmp3; })) catch unreachable; break :blk7 m; }) catch unreachable; } break :blk9 _tmp7.toOwnedSlice() catch unreachable; };
    _json(result);
    test_TPCDS_Q93_active_sales();
}
