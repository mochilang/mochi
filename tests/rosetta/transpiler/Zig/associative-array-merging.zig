// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:01:10Z
const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn merge(base: std.AutoHashMap([]const u8, i32), update: std.AutoHashMap([]const u8, i32)) std.AutoHashMap([]const u8, i32) {
    var result = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
    var _tmp0 = base.keyIterator();
    while (_tmp0.next()) |k_ptr| {
        const k = k_ptr.*;
        _ = result.put(k, base[k]) catch |err| handleError(err);
    }
    var _tmp1 = update.keyIterator();
    while (_tmp1.next()) |k_ptr| {
        const k = k_ptr.*;
        _ = result.put(k, update[k]) catch |err| handleError(err);
    }
    return result;
}

fn user_main() void {
    const base: std.StringHashMap(i32) = struct {
    name: []const u8,
    price: f64,
    color: []const u8,
}{
    .name = "Rocket Skates",
    .price = 12.75,
    .color = "yellow",
}; // std.StringHashMap(i32)
    const update: std.StringHashMap(i32) = struct {
    price: f64,
    color: []const u8,
    year: i32,
}{
    .price = 15.25,
    .color = "red",
    .year = 1974,
}; // std.StringHashMap(i32)
    const result = merge(base, update); // std.StringHashMap(i32)
    std.debug.print("{any}\n", .{result});
}

pub fn main() void {
    user_main();
}
