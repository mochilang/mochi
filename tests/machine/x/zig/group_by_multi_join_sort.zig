const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return sum;
}

fn _print_list(comptime T: type, v: []const T) void {
    for (v, 0..) |it, i| {
        if (i > 0) std.debug.print(" ", .{});
        std.debug.print("{any}", .{it});
    }
    std.debug.print("\n", .{});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var nation: []const std.AutoHashMap([]const u8, i32) = undefined;
var customer: []const std.AutoHashMap([]const u8, i32) = undefined;
var orders: []const std.AutoHashMap([]const u8, i32) = undefined;
var lineitem: []const std.AutoHashMap([]const u8, i32) = undefined;
var start_date: []const u8 = undefined;
var end_date: []const u8 = undefined;
var result: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    nation = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("n_nationkey", 1) catch unreachable; m.put("n_name", "BRAZIL") catch unreachable; break :blk0 m; }};
    customer = &[_]std.AutoHashMap([]const u8, i32){blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_custkey", 1) catch unreachable; m.put("c_name", "Alice") catch unreachable; m.put("c_acctbal", 100) catch unreachable; m.put("c_nationkey", 1) catch unreachable; m.put("c_address", "123 St") catch unreachable; m.put("c_phone", "123-456") catch unreachable; m.put("c_comment", "Loyal") catch unreachable; break :blk1 m; }};
    orders = &[_]std.AutoHashMap([]const u8, i32){blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("o_orderkey", 1000) catch unreachable; m.put("o_custkey", 1) catch unreachable; m.put("o_orderdate", "1993-10-15") catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("o_orderkey", 2000) catch unreachable; m.put("o_custkey", 1) catch unreachable; m.put("o_orderdate", "1994-01-02") catch unreachable; break :blk3 m; }};
    lineitem = &[_]std.AutoHashMap([]const u8, i32){blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("l_orderkey", 1000) catch unreachable; m.put("l_returnflag", "R") catch unreachable; m.put("l_extendedprice", 1000) catch unreachable; m.put("l_discount", 0.1) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("l_orderkey", 2000) catch unreachable; m.put("l_returnflag", "N") catch unreachable; m.put("l_extendedprice", 500) catch unreachable; m.put("l_discount", 0) catch unreachable; break :blk5 m; }};
    start_date = "1993-10-01";
    end_date = "1994-01-01";
    result = blk9: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (customer) |c| { for (orders) |o| { if (!((o.o_custkey == c.c_custkey))) continue; for (lineitem) |l| { if (!((l.l_orderkey == o.o_orderkey))) continue; for (nation) |n| { if (!((n.n_nationkey == c.c_nationkey))) continue; if (!((((o.o_orderdate >= start_date) and (o.o_orderdate < end_date)) and std.mem.eql(u8, l.l_returnflag, "R")))) continue; const _tmp4 = blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_custkey", c.c_custkey) catch unreachable; m.put("c_name", c.c_name) catch unreachable; m.put("c_acctbal", c.c_acctbal) catch unreachable; m.put("c_address", c.c_address) catch unreachable; m.put("c_phone", c.c_phone) catch unreachable; m.put("c_comment", c.c_comment) catch unreachable; m.put("n_name", n.n_name) catch unreachable; break :blk6 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk7: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("c_custkey", g.key.c_custkey) catch unreachable; m.put("c_name", g.key.c_name) catch unreachable; m.put("revenue", _sum_int(blk8: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append((x.l.l_extendedprice * ((1 - x.l.l_discount)))) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk8 _tmp1; })) catch unreachable; m.put("c_acctbal", g.key.c_acctbal) catch unreachable; m.put("n_name", g.key.n_name) catch unreachable; m.put("c_address", g.key.c_address) catch unreachable; m.put("c_phone", g.key.c_phone) catch unreachable; m.put("c_comment", g.key.c_comment) catch unreachable; break :blk7 m; }) catch unreachable; } break :blk9 _tmp5.toOwnedSlice() catch unreachable; };
    _print_list(std.AutoHashMap([]const u8, i32), result);
}
