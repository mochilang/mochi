const std = @import("std");

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

const customers = (blk0: { const _tmp0 = struct { id: i32, name: []const u8, }; const _arr = &[_]_tmp0{_tmp0{ .id = 1, .name = "Alice" }, _tmp0{ .id = 2, .name = "Bob" }, _tmp0{ .id = 3, .name = "Charlie" }}; break :blk0 _arr; });
const orders = (blk1: { const _tmp1 = struct { id: i32, customerId: i32, }; const _arr = &[_]_tmp1{_tmp1{ .id = 100, .customerId = 1 }, _tmp1{ .id = 101, .customerId = 1 }, _tmp1{ .id = 102, .customerId = 2 }}; break :blk1 _arr; });
const stats = blk3: { var _tmp4 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { id: i32, name: []const u8, }) }).init(std.heap.page_allocator); var _tmp5 = std.AutoHashMap([]const u8, usize).init(std.heap.page_allocator); for (customers) |c| { for (orders) |o| { if (!((o.customerId == c.id))) continue; const _tmp6 = c.name; if (_tmp5.get(_tmp6)) |idx| { _tmp4.items[idx].Items.append(c) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(struct { id: i32, name: []const u8, }) }{ .key = _tmp6, .Items = std.ArrayList(struct { id: i32, name: []const u8, }).init(std.heap.page_allocator) }; g.Items.append(c) catch unreachable; _tmp4.append(g) catch unreachable; _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable; } } } var _tmp7 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct { id: i32, name: []const u8, }) }).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp7.append(g) catch unreachable; } var _tmp8 = std.ArrayList(struct { name: i32, count: i32, }).init(std.heap.page_allocator);for (_tmp7.items) |g| { _tmp8.append(struct { name: i32, count: i32, }{ .name = g.key, .count = (blk2: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |r| { if (!(r.o)) continue; _tmp2.append(r) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk2 _tmp3; }).len }) catch unreachable; } const _tmp8Slice = _tmp8.toOwnedSlice() catch unreachable; break :blk3 _tmp8Slice; };

pub fn main() void {
    std.debug.print("{s}\n", .{"--- Group Left Join ---"});
    for (stats) |s| {
        std.debug.print("{any} {s} {any}\n", .{s.name, "orders:", s.count});
    }
}
