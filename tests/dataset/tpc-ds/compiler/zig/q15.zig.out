const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _contains_list_string(v: []const []const u8, item: []const u8) bool {
    for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }
    return false;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

fn _slice_string(s: []const u8, start: i32, end: i32, step: i32) []const u8 {
    var sidx = start;
    var eidx = end;
    var stp = step;
    const n: i32 = @as(i32, @intCast(s.len));
    if (sidx < 0) sidx += n;
    if (eidx < 0) eidx += n;
    if (stp == 0) stp = 1;
    if (sidx < 0) sidx = 0;
    if (eidx > n) eidx = n;
    if (stp > 0 and eidx < sidx) eidx = sidx;
    if (stp < 0 and eidx > sidx) eidx = sidx;
    var res = std.ArrayList(u8).init(std.heap.page_allocator);
    defer res.deinit();
    var i: i32 = sidx;
    while ((stp > 0 and i < eidx) or (stp < 0 and i > eidx)) : (i += stp) {
        res.append(s[@as(usize, @intCast(i))]) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const CatalogSale = struct {
    cs_bill_customer_sk: i32,
    cs_sales_price: f64,
    cs_sold_date_sk: i32,
};

const Customer = struct {
    c_customer_sk: i32,
    c_current_addr_sk: i32,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_zip: []const u8,
    ca_state: []const u8,
};

const DateDim = struct {
    d_date_sk: i32,
    d_qoy: i32,
    d_year: i32,
};

var catalog_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var date_dim: []const std.AutoHashMap([]const u8, i32) = undefined;
var filtered: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q15_zip() void {
    expect((filtered == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("ca_zip", "85669") catch unreachable; m.put("sum_sales", 600) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    catalog_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cs_bill_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("cs_sales_price", 600) catch unreachable; m.put("cs_sold_date_sk", @as(i32,@intCast(1))) catch unreachable; break :blk1 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; break :blk2 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_zip", "85669") catch unreachable; m.put("ca_state", "CA") catch unreachable; break :blk3 m; }};
    date_dim = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("d_date_sk", @as(i32,@intCast(1))) catch unreachable; m.put("d_qoy", @as(i32,@intCast(1))) catch unreachable; m.put("d_year", @as(i32,@intCast(2000))) catch unreachable; break :blk4 m; }};
    filtered = blk8: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (catalog_sales) |cs| { for (customer) |c| { if (!((cs.cs_bill_customer_sk == c.c_customer_sk))) continue; for (customer_address) |ca| { if (!((c.c_current_addr_sk == ca.ca_address_sk))) continue; for (date_dim) |d| { if (!((cs.cs_sold_date_sk == d.d_date_sk))) continue; if (!((((((_contains_list_string(&[_][]const u8{"85669", "86197", "88274", "83405", "86475", "85392", "85460", "80348", "81792"}, _slice_string(ca.ca_zip, @as(i32,@intCast(0)), @as(i32,@intCast(5)), 1)) or _contains_list_string(&[_][]const u8{"CA", "WA", "GA"}, ca.ca_state)) or (cs.cs_sales_price > @as(i32,@intCast(500))))) and (d.d_qoy == @as(i32,@intCast(1)))) and (d.d_year == @as(i32,@intCast(2000)))))) continue; const _tmp4 = blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("zip", ca.ca_zip) catch unreachable; break :blk5 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(cs) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(cs) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_zip", g.key.zip) catch unreachable; m.put("sum_sales", _sum_int(blk7: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.cs_sales_price) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; })) catch unreachable; break :blk6 m; }) catch unreachable; } break :blk8 _tmp5.toOwnedSlice() catch unreachable; };
    _json(filtered);
    test_TPCDS_Q15_zip();
}
