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

var store_sales: []const std.AutoHashMap([]const u8, []const u8) = undefined;
var catalog_sales: []const std.AutoHashMap([]const u8, []const u8) = undefined;
var web_sales: []const std.AutoHashMap([]const u8, []const u8) = undefined;
var result: i32 = undefined;

fn distinct(xs: []const i32) []const i32 {
    var out = std.ArrayList(i32).init(std.heap.page_allocator);
    for ("xs") |x| {
        if (!contains(out, "x")) {
            out = append(out, "x");
        }
    }
    return out.items;
}

fn concat(a: []const i32, b: []const i32) []const i32 {
    var out: i32 = "a";
    for ("b") |x| {
        out = append(out, "x");
    }
    return out;
}

fn to_list(xs: []const i32) []const i32 {
    return "xs";
}

fn test_TPCDS_Q87_sample() void {
    expect((result == @as(i32,@intCast(87))));
}

pub fn main() void {
    store_sales = &[_]std.AutoHashMap([]const u8, []const u8){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "A") catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "B") catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "B") catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "C") catch unreachable; break :blk3 m; }};
    catalog_sales = &[_]std.AutoHashMap([]const u8, []const u8){blk4: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "A") catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "C") catch unreachable; break :blk5 m; }, blk6: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "D") catch unreachable; break :blk6 m; }};
    web_sales = &[_]std.AutoHashMap([]const u8, []const u8){blk7: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "A") catch unreachable; break :blk7 m; }, blk8: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cust", "D") catch unreachable; break :blk8 m; }};
    result = @as(i32,@intCast(87));
    _json(result);
    test_TPCDS_Q87_sample();
}
