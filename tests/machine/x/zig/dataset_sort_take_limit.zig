const std = @import("std");

fn _slice_list(comptime T: type, v: []const T, start: i32, end: i32, step: i32) []T {
    var s = start;
    var e = end;
    var st = step;
    const n: i32 = @as(i32, @intCast(v.len));
    if (s < 0) s += n;
    if (e < 0) e += n;
    if (st == 0) st = 1;
    if (s < 0) s = 0;
    if (e > n) e = n;
    if (st > 0 and e < s) e = s;
    if (st < 0 and e > s) e = s;
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    var i: i32 = s;
    while ((st > 0 and i < e) or (st < 0 and i > e)) : (i += st) {
        res.append(v[@as(usize, @intCast(i))]) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

const products = (blk0: { const _tmp0 = struct {
    name: []const u8,
    price: i32,
}; const _arr = &[_]_tmp0{
    _tmp0{
    .name = "Laptop",
    .price = 1500,
},
    _tmp0{
    .name = "Smartphone",
    .price = 900,
},
    _tmp0{
    .name = "Tablet",
    .price = 600,
},
    _tmp0{
    .name = "Monitor",
    .price = 300,
},
    _tmp0{
    .name = "Keyboard",
    .price = 100,
},
    _tmp0{
    .name = "Mouse",
    .price = 50,
},
    _tmp0{
    .name = "Headphones",
    .price = 200,
},
}; break :blk0 _arr; });
const expensive = blk1: { var _tmp1 = std.ArrayList(struct { item: struct {
    name: []const u8,
    price: i32,
}, key: i32 }).init(std.heap.page_allocator); for (products) |p| { _tmp1.append(.{ .item = p, .key = -p.price }) catch unreachable; } for (0.._tmp1.items.len) |i| { for (i+1.._tmp1.items.len) |j| { if (_tmp1.items[j].key < _tmp1.items[i].key) { const t = _tmp1.items[i]; _tmp1.items[i] = _tmp1.items[j]; _tmp1.items[j] = t; } } } var _tmp2 = std.ArrayList(struct {
    name: []const u8,
    price: i32,
}).init(std.heap.page_allocator);for (_tmp1.items) |p| { _tmp2.append(p.item) catch unreachable; } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; _tmp3 = _slice_list(struct {
    name: []const u8,
    price: i32,
}, _tmp3, 1, (1 + 3), 1); break :blk1 _tmp3; };

pub fn main() void {
    std.debug.print("--- Top products (excluding most expensive) ---\n", .{});
    for (expensive) |item| {
        std.debug.print("{s} {s} {d}\n", .{item.name, "costs $", item.price});
    }
}
