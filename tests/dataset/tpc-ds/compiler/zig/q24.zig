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
    ss_ticket_number: i32,
    ss_item_sk: i32,
    ss_customer_sk: i32,
    ss_store_sk: i32,
    ss_net_paid: f64,
};

const StoreReturn = struct {
    sr_ticket_number: i32,
    sr_item_sk: i32,
};

const Store = struct {
    s_store_sk: i32,
    s_store_name: []const u8,
    s_market_id: i32,
    s_state: []const u8,
    s_zip: []const u8,
};

const Item = struct {
    i_item_sk: i32,
    i_color: []const u8,
    i_current_price: f64,
    i_manager_id: i32,
    i_units: []const u8,
    i_size: []const u8,
};

const Customer = struct {
    c_customer_sk: i32,
    c_first_name: []const u8,
    c_last_name: []const u8,
    c_current_addr_sk: i32,
    c_birth_country: []const u8,
};

const CustomerAddress = struct {
    ca_address_sk: i32,
    ca_state: []const u8,
    ca_country: []const u8,
    ca_zip: []const u8,
};

var store_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var store_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var store: []const std.AutoHashMap([]const u8, i32) = undefined;
var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer_address: []const std.AutoHashMap([]const u8, i32) = undefined;
var ssales: []const std.AutoHashMap([]const u8, i32) = undefined;
var avg_paid: f64 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

fn test_TPCDS_Q24_customer_net_paid() void {
    expect((result == &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("c_last_name", "Smith") catch unreachable; m.put("c_first_name", "Ann") catch unreachable; m.put("s_store_name", "Store1") catch unreachable; m.put("paid", 100) catch unreachable; break :blk0 m; }}));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_net_paid", 100) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ss_ticket_number", @as(i32,@intCast(2))) catch unreachable; m.put("ss_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ss_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ss_net_paid", 50) catch unreachable; break :blk2 m; }};
    store_returns = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_ticket_number", @as(i32,@intCast(1))) catch unreachable; m.put("sr_item_sk", @as(i32,@intCast(1))) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("sr_ticket_number", @as(i32,@intCast(2))) catch unreachable; m.put("sr_item_sk", @as(i32,@intCast(2))) catch unreachable; break :blk4 m; }};
    store = &[_]std.AutoHashMap([]const u8, i32){blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("s_store_sk", @as(i32,@intCast(1))) catch unreachable; m.put("s_store_name", "Store1") catch unreachable; m.put("s_market_id", @as(i32,@intCast(5))) catch unreachable; m.put("s_state", "CA") catch unreachable; m.put("s_zip", "12345") catch unreachable; break :blk5 m; }};
    item = &[_]std.AutoHashMap([]const u8, i32){blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(1))) catch unreachable; m.put("i_color", "RED") catch unreachable; m.put("i_current_price", 10) catch unreachable; m.put("i_manager_id", @as(i32,@intCast(1))) catch unreachable; m.put("i_units", "EA") catch unreachable; m.put("i_size", "M") catch unreachable; break :blk6 m; }, blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("i_item_sk", @as(i32,@intCast(2))) catch unreachable; m.put("i_color", "BLUE") catch unreachable; m.put("i_current_price", 20) catch unreachable; m.put("i_manager_id", @as(i32,@intCast(2))) catch unreachable; m.put("i_units", "EA") catch unreachable; m.put("i_size", "L") catch unreachable; break :blk7 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk8: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_first_name", "Ann") catch unreachable; m.put("c_last_name", "Smith") catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(1))) catch unreachable; m.put("c_birth_country", "Canada") catch unreachable; break :blk8 m; }, blk9: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_customer_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_first_name", "Bob") catch unreachable; m.put("c_last_name", "Jones") catch unreachable; m.put("c_current_addr_sk", @as(i32,@intCast(2))) catch unreachable; m.put("c_birth_country", "USA") catch unreachable; break :blk9 m; }};
    customer_address = &[_]std.AutoHashMap([]const u8, i32){blk10: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(1))) catch unreachable; m.put("ca_state", "CA") catch unreachable; m.put("ca_country", "USA") catch unreachable; m.put("ca_zip", "12345") catch unreachable; break :blk10 m; }, blk11: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("ca_address_sk", @as(i32,@intCast(2))) catch unreachable; m.put("ca_state", "CA") catch unreachable; m.put("ca_country", "USA") catch unreachable; m.put("ca_zip", "54321") catch unreachable; break :blk11 m; }};
    ssales = blk15: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (store_sales) |ss| { for (store_returns) |sr| { if (!(((ss.ss_ticket_number == sr.sr_ticket_number) and (ss.ss_item_sk == sr.sr_item_sk)))) continue; for (store) |s| { if (!((ss.ss_store_sk == s.s_store_sk))) continue; for (item) |i| { if (!((ss.ss_item_sk == i.i_item_sk))) continue; for (customer) |c| { if (!((ss.ss_customer_sk == c.c_customer_sk))) continue; for (customer_address) |ca| { if (!((c.c_current_addr_sk == ca.ca_address_sk))) continue; if (!((((c.c_birth_country != strings.ToUpper(ca.ca_country)) and (s.s_zip == ca.ca_zip)) and (s.s_market_id == @as(i32,@intCast(5)))))) continue; const _tmp4 = blk12: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("last", c.c_last_name) catch unreachable; m.put(first, c.c_first_name) catch unreachable; m.put("store_name", s.s_store_name) catch unreachable; m.put("color", i.i_color) catch unreachable; break :blk12 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(ss) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(ss) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk13: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_last_name", g.key.last) catch unreachable; m.put("c_first_name", g.key.first) catch unreachable; m.put("s_store_name", g.key.store_name) catch unreachable; m.put("color", g.key.color) catch unreachable; m.put("netpaid", _sum_int(blk14: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.ss_net_paid) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk14 _tmp1; })) catch unreachable; break :blk13 m; }) catch unreachable; } break :blk15 _tmp5.toOwnedSlice() catch unreachable; };
    avg_paid = _avg_int(blk16: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (ssales) |x| { _tmp6.append(x.netpaid) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk16 _tmp7; });
    result = blk18: { var _tmp8 = std.ArrayList(struct { item: std.AutoHashMap([]const u8, i32), key: []const i32 }).init(std.heap.page_allocator); for (ssales) |x| { if (!((std.mem.eql(u8, x.color, "RED") and (x.netpaid > (0.05 * avg_paid))))) continue; _tmp8.append(.{ .item = blk17: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_last_name", x.c_last_name) catch unreachable; m.put("c_first_name", x.c_first_name) catch unreachable; m.put("s_store_name", x.s_store_name) catch unreachable; m.put("paid", x.netpaid) catch unreachable; break :blk17 m; }, .key = &[_]i32{x.c_last_name, x.c_first_name, x.s_store_name} }) catch unreachable; } for (0.._tmp8.items.len) |i| { for (i+1.._tmp8.items.len) |j| { if (_tmp8.items[j].key < _tmp8.items[i].key) { const t = _tmp8.items[i]; _tmp8.items[i] = _tmp8.items[j]; _tmp8.items[j] = t; } } } var _tmp9 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp8.items) |p| { _tmp9.append(p.item) catch unreachable; } const _tmp10 = _tmp9.toOwnedSlice() catch unreachable; break :blk18 _tmp10; };
    _json(result);
    test_TPCDS_Q24_customer_net_paid();
}
