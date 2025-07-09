const std = @import("std");

const customers = (blk0: { const _tmp0 = struct { id: i32, name: []const u8, }; const _arr = &[_]_tmp0{_tmp0{ .id = 1, .name = "Alice" }, _tmp0{ .id = 2, .name = "Bob" }, _tmp0{ .id = 3, .name = "Charlie" }, _tmp0{ .id = 4, .name = "Diana" }}; break :blk0 _arr; });
const orders = (blk1: { const _tmp1 = struct { id: i32, customerId: i32, total: i32, }; const _arr = &[_]_tmp1{_tmp1{ .id = 100, .customerId = 1, .total = 250 }, _tmp1{ .id = 101, .customerId = 2, .total = 125 }, _tmp1{ .id = 102, .customerId = 1, .total = 300 }, _tmp1{ .id = 103, .customerId = 5, .total = 80 }}; break :blk1 _arr; });
const result = blk2: { var _tmp2 = std.ArrayList(struct { order: struct { id: i32 customerId: i32 total: i32 } customer: struct { id: i32 name: []const u8 } }).init(std.heap.page_allocator); for (orders) |o| { for (customers) |c| { if (!((o.customerId == c.id))) continue; _tmp2.append(struct { order: struct { id: i32 customerId: i32 total: i32 }, customer: struct { id: i32 name: []const u8 }, }{ .order = o, .customer = c }) catch unreachable; } } const _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk2 _tmp3; };

pub fn main() void {
    std.debug.print("{s}\n", .{"--- Outer Join using syntax ---"});
    for (result) |row| {
        if (row.order) {
            if (row.customer) {
                std.debug.print("{s} {any} {s} {any} {s} {any}\n", .{"Order", row.order.id, "by", row.customer.name, "- $", row.order.total});
            } else {
                std.debug.print("{s} {any} {s} {s} {s} {any}\n", .{"Order", row.order.id, "by", "Unknown", "- $", row.order.total});
            }
        } else {
            std.debug.print("{s} {any} {s}\n", .{"Customer", row.customer.name, "has no orders"});
        }
    }
}
