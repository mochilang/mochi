const std = @import("std");

fn _contains(comptime T: type, v: []const T, item: T) bool {
    for (v) |it| {
        if (std.meta.eql(it, item)) return true;
    }
    return false;
}

fn _union_all(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| {
        res.append(it) catch unreachable;
    }
    for (b) |it| {
        res.append(it) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

fn _union(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| {
        res.append(it) catch unreachable;
    }
    for (b) |it| {
        if (!_contains(T, res.items, it)) res.append(it) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

fn _except(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| {
        if (!_contains(T, b, it)) res.append(it) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

fn _intersect(comptime T: type, a: []const T, b: []const T) []T {
    var res = std.ArrayList(T).init(std.heap.page_allocator);
    defer res.deinit();
    for (a) |it| {
        if (_contains(T, b, it) and !_contains(T, res.items, it)) res.append(it) catch unreachable;
    }
    return res.toOwnedSlice() catch unreachable;
}

pub fn main() void {
    std.debug.print("{any}\n", .{_union(i32, &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)) }, &[_]i32{ @as(i32, @intCast(2)), @as(i32, @intCast(3)) })});
    std.debug.print("{any}\n", .{_except(i32, &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)) }, &[_]i32{@as(i32, @intCast(2))})});
    std.debug.print("{any}\n", .{_intersect(i32, &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)), @as(i32, @intCast(3)) }, &[_]i32{ @as(i32, @intCast(2)), @as(i32, @intCast(4)) })});
    std.debug.print("{any}\n", .{(_union_all(i32, &[_]i32{ @as(i32, @intCast(1)), @as(i32, @intCast(2)) }, &[_]i32{ @as(i32, @intCast(2)), @as(i32, @intCast(3)) })).len});
}
