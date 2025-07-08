const std = @import("std");

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var customers: []const std.AutoHashMap([]const u8, i32) = undefined;
var orders: []const std.AutoHashMap([]const u8, i32) = undefined;
var stats: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    customers = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 1) catch unreachable; m.put("name", "Alice") catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 2) catch unreachable; m.put("name", "Bob") catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 3) catch unreachable; m.put("name", "Charlie") catch unreachable; break :blk2 m; }};
    orders = &[_]std.AutoHashMap([]const u8, i32){blk3: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 100) catch unreachable; m.put("customerId", 1) catch unreachable; break :blk3 m; }, blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 101) catch unreachable; m.put("customerId", 1) catch unreachable; break :blk4 m; }, blk5: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("id", 102) catch unreachable; m.put("customerId", 2) catch unreachable; break :blk5 m; }};
    stats = blk8: { var _tmp2 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator); for (customers) |c| { for (orders) |o| { if (!((o.customerId == c.id))) continue; const _tmp4 = c.name; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(blk6: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("name", g.key) catch unreachable; m.put(count, (blk7: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |r| { if (!(r.o)) continue; _tmp0.append(r) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk7 _tmp1; }).len) catch unreachable; break :blk6 m; }) catch unreachable; } break :blk8 _tmp5.toOwnedSlice() catch unreachable; };
    std.debug.print("{s}\n", .{"--- Group Left Join ---"});
    for (stats) |s| {
        std.debug.print("{any} {s} {any}\n", .{s.name, "orders:", s.count});
    }
}
