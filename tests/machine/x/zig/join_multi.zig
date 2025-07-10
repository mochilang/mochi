const std = @import("std");

const customers = (blk0: { const _tmp0 = struct {
    id: i32,
    name: []const u8,
}; const _arr = &[_]_tmp0{
    _tmp0{
    .id = 1,
    .name = "Alice",
},
    _tmp0{
    .id = 2,
    .name = "Bob",
},
}; break :blk0 _arr; });
const orders = (blk1: { const _tmp1 = struct {
    id: i32,
    customerId: i32,
}; const _arr = &[_]_tmp1{
    _tmp1{
    .id = 100,
    .customerId = 1,
},
    _tmp1{
    .id = 101,
    .customerId = 2,
},
}; break :blk1 _arr; });
const items = (blk2: { const _tmp2 = struct {
    orderId: i32,
    sku: []const u8,
}; const _arr = &[_]_tmp2{
    _tmp2{
    .orderId = 100,
    .sku = "a",
},
    _tmp2{
    .orderId = 101,
    .sku = "b",
},
}; break :blk2 _arr; });
const result = blk3: { var _tmp3 = std.ArrayList(struct {
    name: []const u8,
    sku: []const u8,
}).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { if (!((o.customerId == c.id))) continue; for (items) |i| { if (!((o.id == i.orderId))) continue; _tmp3.append(struct {
    name: []const u8,
    sku: []const u8,
}{
    .name = c.name,
    .sku = i.sku,
}) catch unreachable; } } } const _tmp4 = _tmp3.toOwnedSlice() catch unreachable; break :blk3 _tmp4; };

pub fn main() void {
    std.debug.print("{s}\n", .{"--- Multi Join ---"});
    for (result) |r| {
        std.debug.print("{s} {s} {s}\n", .{r.name, "bought item", r.sku});
    }
}
