const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

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
        res.append(v[@as(usize, @intCast(i))]) catch |err| handleError(err);
    }
    return res.toOwnedSlice() catch |err| handleError(err);
}

const ProductsItem = struct {
    name: []const u8,
    price: i32,
};
const products = &[_]ProductsItem{
    ProductsItem{
    .name = "Laptop",
    .price = 1500,
},
    ProductsItem{
    .name = "Smartphone",
    .price = 900,
},
    ProductsItem{
    .name = "Tablet",
    .price = 600,
},
    ProductsItem{
    .name = "Monitor",
    .price = 300,
},
    ProductsItem{
    .name = "Keyboard",
    .price = 100,
},
    ProductsItem{
    .name = "Mouse",
    .price = 50,
},
    ProductsItem{
    .name = "Headphones",
    .price = 200,
},
}; // []const ProductsItem
const expensive = blk0: { var _tmp0 = std.ArrayList(struct { item: ProductsItem, key: i32 }).init(std.heap.page_allocator); for (products) |p| { _tmp0.append(.{ .item = p, .key = -p.price }) catch |err| handleError(err); } for (0.._tmp0.items.len) |i| { for (i+1.._tmp0.items.len) |j| { if (_tmp0.items[j].key < _tmp0.items[i].key) { const t = _tmp0.items[i]; _tmp0.items[i] = _tmp0.items[j]; _tmp0.items[j] = t; } } } var _tmp1 = std.ArrayList(ProductsItem).init(std.heap.page_allocator);for (_tmp0.items) |p| { _tmp1.append(p.item) catch |err| handleError(err); } const _tmp2 = _tmp1.toOwnedSlice() catch |err| handleError(err); _tmp2 = _slice_list(ProductsItem, _tmp2, 1, (1 + 3), 1); break :blk0 _tmp2; }; // []const ProductsItem

pub fn main() void {
    std.debug.print("--- Top products (excluding most expensive) ---\n", .{});
    for (expensive) |item| {
        std.debug.print("{s} {s} {d}\n", .{item.name, "costs $", item.price});
    }
}
