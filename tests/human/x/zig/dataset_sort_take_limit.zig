const std = @import("std");

const Product = struct {
    name: []const u8,
    price: i32,
};

pub fn main() !void {
    var products = [_]Product{
        .{ .name = "Laptop", .price = 1500 },
        .{ .name = "Smartphone", .price = 900 },
        .{ .name = "Tablet", .price = 600 },
        .{ .name = "Monitor", .price = 300 },
        .{ .name = "Keyboard", .price = 100 },
        .{ .name = "Mouse", .price = 50 },
        .{ .name = "Headphones", .price = 200 },
    };

    var slice = products[0..];
    std.sort.sort(Product, slice, {},
        comptime fn (ctx: void, a: Product, b: Product) bool {
            _ = ctx;
            return a.price > b.price; // descending
        });

    const expensive = slice[1..4];

    std.debug.print("--- Top products (excluding most expensive) ---\n", .{});
    for (expensive) |item| {
        std.debug.print("{s} costs ${d}\n", .{ item.name, item.price });
    }
}

