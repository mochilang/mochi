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

var catalog_returns: []const std.AutoHashMap([]const u8, i32) = undefined;
var avg_list: []const std.AutoHashMap([]const u8, i32) = undefined;
var avg_state: i32 = undefined;
var result_list: []const i32 = undefined;
var result: i32 = undefined;

fn test_TPCDS_Q81_sample() void {
    expect((result == 81));
}

pub fn main() void {
    catalog_returns = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cust", @as(i32,@intCast(1))) catch unreachable; m.put("state", "CA") catch unreachable; m.put("amt", 40) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cust", @as(i32,@intCast(2))) catch unreachable; m.put("state", "CA") catch unreachable; m.put("amt", 50) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cust", @as(i32,@intCast(3))) catch unreachable; m.put("state", "CA") catch unreachable; m.put("amt", 81) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cust", @as(i32,@intCast(4))) catch unreachable; m.put("state", "TX") catch unreachable; m.put("amt", 30) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("cust", @as(i32,@intCast(5))) catch unreachable; m.put("state", "TX") catch unreachable; m.put("amt", 20) catch unreachable; break :blk4 m; }};
    avg_list = blk7: { var _tmp2 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (catalog_returns) |r| { const _tmp4 = r.state; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(r) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(r) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("state", g.key) catch unreachable; m.put("avg_amt", _avg_int(blk6: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |x| { _tmp0.append(x.amt) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk6 _tmp1; })) catch unreachable; break :blk5 m; }) catch unreachable; } break :blk7 _tmp5.toOwnedSlice() catch unreachable; };
    avg_state = first(blk8: { var _tmp6 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (avg_list) |a| { if (!(std.mem.eql(u8, a.state, "CA"))) continue; _tmp6.append(a) catch unreachable; } const _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk8 _tmp7; });
    result_list = blk9: { var _tmp8 = std.ArrayList(i32).init(std.heap.page_allocator); for (catalog_returns) |r| { if (!((std.mem.eql(u8, r.state, "CA") and (r.amt > (avg_state.avg_amt * 1.2))))) continue; _tmp8.append(r.amt) catch unreachable; } const _tmp9 = _tmp8.toOwnedSlice() catch unreachable; break :blk9 _tmp9; };
    result = first(result_list);
    _json(result);
    test_TPCDS_Q81_sample();
}
