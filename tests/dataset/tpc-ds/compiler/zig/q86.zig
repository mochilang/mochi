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

var web_sales: []const std.AutoHashMap([]const u8, i32) = undefined;
var result: f64 = undefined;

fn test_TPCDS_Q86_sample() void {
    expect((result == 86));
}

pub fn main() void {
    web_sales = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cat", "A") catch unreachable; m.put("class", "B") catch unreachable; m.put("net", 40) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cat", "A") catch unreachable; m.put("class", "B") catch unreachable; m.put("net", 46) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cat", "A") catch unreachable; m.put("class", "C") catch unreachable; m.put("net", 10) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("cat", "B") catch unreachable; m.put("class", "B") catch unreachable; m.put("net", 20) catch unreachable; break :blk3 m; }};
    result = _sum_int(blk4: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (web_sales) |ws| { if (!((std.mem.eql(u8, ws.cat, "A") and std.mem.eql(u8, ws.class, "B")))) continue; _tmp0.append(ws.net) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk4 _tmp1; });
    _json(result);
    test_TPCDS_Q86_sample();
}
