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

var item: []const std.AutoHashMap([]const u8, i32) = undefined;
var lower: i32 = undefined;
var result: []const i32 = undefined;

fn test_TPCDS_Q41_simplified() void {
    expect((result == &[_][]const u8{"Blue Shirt", "Red Dress"}));
}

pub fn main() void {
    item = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("product_name", "Blue Shirt") catch unreachable; m.put("manufact_id", @as(i32,@intCast(100))) catch unreachable; m.put("manufact", @as(i32,@intCast(1))) catch unreachable; m.put("category", "Women") catch unreachable; m.put("color", "blue") catch unreachable; m.put("units", "pack") catch unreachable; m.put("size", "M") catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("product_name", "Red Dress") catch unreachable; m.put("manufact_id", @as(i32,@intCast(120))) catch unreachable; m.put("manufact", @as(i32,@intCast(1))) catch unreachable; m.put("category", "Women") catch unreachable; m.put("color", "red") catch unreachable; m.put("units", "pack") catch unreachable; m.put("size", "M") catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("product_name", "Pants") catch unreachable; m.put("manufact_id", @as(i32,@intCast(200))) catch unreachable; m.put("manufact", @as(i32,@intCast(2))) catch unreachable; m.put("category", "Men") catch unreachable; m.put("color", "black") catch unreachable; m.put("units", "pair") catch unreachable; m.put("size", "L") catch unreachable; break :blk2 m; }};
    lower = @as(i32,@intCast(100));
    result = blk4: { var _tmp2 = std.ArrayList(struct { item: i32, key: i32 }).init(std.heap.page_allocator); for (item) |_i1| { if (!((((_i1.manufact_id >= lower) and (_i1.manufact_id <= (lower + @as(i32,@intCast(40))))) and ((blk3: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (item) |_i2| { if (!(((_i2.manufact == _i1.manufact) and (_i2.category == _i1.category)))) continue; _tmp0.append(_i2) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk3 _tmp1; }).len > @as(i32,@intCast(1)))))) continue; _tmp2.append(.{ .item = _i1.product_name, .key = _i1.product_name }) catch unreachable; } for (0.._tmp2.items.len) |i| { for (i+1.._tmp2.items.len) |j| { if (_tmp2.items[j].key < _tmp2.items[i].key) { const t = _tmp2.items[i]; _tmp2.items[i] = _tmp2.items[j]; _tmp2.items[j] = t; } } } var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator);for (_tmp2.items) |p| { _tmp3.append(p.item) catch unreachable; } const _tmp4 = _tmp3.toOwnedSlice() catch unreachable; break :blk4 _tmp4; };
    _json(result);
    test_TPCDS_Q41_simplified();
}
