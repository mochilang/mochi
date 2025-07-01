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

var sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var by_mgr: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q63_simplified() void {
    expect((result == @as(i32,@intCast(63))));
}

pub fn main() void {
    sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("mgr", @as(i32,@intCast(1))) catch unreachable; m.put("amount", @as(i32,@intCast(30))) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("mgr", @as(i32,@intCast(2))) catch unreachable; m.put("amount", @as(i32,@intCast(33))) catch unreachable; break :blk1 m; }};
    by_mgr = blk5: { var _tmp2 = std.ArrayList(struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(std.AutoHashMap([]const u8, i32), usize).init(std.heap.page_allocator); for (sales) |s| { const _tmp4 = blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("mgr", s.mgr) catch unreachable; break :blk2 m; }; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(s) catch unreachable; } else { var g = struct { key: std.AutoHashMap([]const u8, i32), Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(s) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("mgr", g.key.mgr) catch unreachable; m.put("sum_sales", _sum_int(blk4: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.amount) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk4 _tmp1; })) catch unreachable; break :blk3 m; }) catch unreachable; } break :blk5 _tmp5.toOwnedSlice() catch unreachable; };
    result = _sum_int(blk6: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (by_mgr) |x| { _tmp6.append(x.sum_sales) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk6 _tmp7; });
    _json(result);
    test_TPCDS_Q63_simplified();
}
