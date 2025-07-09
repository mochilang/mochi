const std = @import("std");

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

const people = (blk0: { const _tmp0 = struct { name: []const u8, city: []const u8, }; const _arr = &[_]_tmp0{_tmp0{ .name = "Alice", .city = "Paris" }, _tmp0{ .name = "Bob", .city = "Hanoi" }, _tmp0{ .name = "Charlie", .city = "Paris" }, _tmp0{ .name = "Diana", .city = "Hanoi" }, _tmp0{ .name = "Eve", .city = "Paris" }, _tmp0{ .name = "Frank", .city = "Hanoi" }, _tmp0{ .name = "George", .city = "Paris" }}; break :blk0 _arr; });
const big = blk1: { var _tmp1 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { name: []const u8 city: []const u8 }) }).init(std.heap.page_allocator); var _tmp2 = std.AutoHashMap([]const u8, usize).init(std.heap.page_allocator); for (people) |p| { const _tmp3 = p.city; if (_tmp2.get(_tmp3)) |idx| { _tmp1.items[idx].Items.append(p) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(struct { name: []const u8 city: []const u8 }) }{ .key = _tmp3, .Items = std.ArrayList(struct { name: []const u8 city: []const u8 }).init(std.heap.page_allocator) }; g.Items.append(p) catch unreachable; _tmp1.append(g) catch unreachable; _tmp2.put(_tmp3, _tmp1.items.len - 1) catch unreachable; } } var _tmp4 = std.ArrayList(struct { city: i32 num: i32 }).init(std.heap.page_allocator);for (_tmp1.items) |g| { _tmp4.append(struct { city: i32, num: i32, }{ .city = g.key, .num = (g.Items.len) }) catch unreachable; } break :blk1 _tmp4.toOwnedSlice() catch unreachable; };

pub fn main() void {
    _json(big);
}
