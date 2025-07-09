const std = @import("std");

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const customers = (blk0: { const _tmp0 = struct { id: i32, name: []const u8, }; const _arr = &[_]_tmp0{_tmp0{ .id = 1, .name = "Alice" }, _tmp0{ .id = 2, .name = "Bob" }}; break :blk0 _arr; });
const orders = (blk1: { const _tmp1 = struct { id: i32, customerId: i32, }; const _arr = &[_]_tmp1{_tmp1{ .id = 100, .customerId = 1 }, _tmp1{ .id = 101, .customerId = 1 }, _tmp1{ .id = 102, .customerId = 2 }}; break :blk1 _arr; });
const stats = blk2: { var _tmp2 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { id: i32 customerId: i32 }) }).init(std.heap.page_allocator); var _tmp3 = std.AutoHashMap([]const u8, usize).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { if (!((o.customerId == c.id))) continue; const _tmp4 = c.name; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(o) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(struct { id: i32 customerId: i32 }) }{ .key = _tmp4, .Items = std.ArrayList(struct { id: i32 customerId: i32 }).init(std.heap.page_allocator) }; g.Items.append(o) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } } var _tmp5 = std.ArrayList(struct { name: i32 count: i32 }).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(struct { name: i32, count: i32, }{ .name = g.key, .count = (g.Items.len) }) catch unreachable; } break :blk2 _tmp5.toOwnedSlice() catch unreachable; };

pub fn main() void {
    std.debug.print("{s}\n", .{"--- Orders per customer ---"});
    for (stats) |s| {
        std.debug.print("{any} {s} {any}\n", .{s.name, "orders:", s.count});
    }
}
