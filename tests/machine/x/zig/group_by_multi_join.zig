const std = @import("std");

fn _sum_int(v: []const i32) i32 {
    var sum: i32 = 0;
    for (v) |it| {
        sum += it;
    }
    return sum;
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var nations: []const std.AutoHashMap([]const u8, i32) = undefined;
var suppliers: []const std.AutoHashMap([]const u8, i32) = undefined;
var partsupp: []const std.AutoHashMap([]const u8, i32) = undefined;
var filtered: []const std.AutoHashMap([]const u8, i32) = undefined;
var grouped: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    nations = &[_]std.AutoHashMap([]const u8, i32){ blk0: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(1))) catch unreachable;
        m.put("name", "A") catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(2))) catch unreachable;
        m.put("name", "B") catch unreachable;
        break :blk1 m;
    } };
    suppliers = &[_]std.AutoHashMap([]const u8, i32){ blk2: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(1))) catch unreachable;
        m.put("nation", @as(i32, @intCast(1))) catch unreachable;
        break :blk2 m;
    }, blk3: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("id", @as(i32, @intCast(2))) catch unreachable;
        m.put("nation", @as(i32, @intCast(2))) catch unreachable;
        break :blk3 m;
    } };
    partsupp = &[_]std.AutoHashMap([]const u8, i32){ blk4: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("part", @as(i32, @intCast(100))) catch unreachable;
        m.put("supplier", @as(i32, @intCast(1))) catch unreachable;
        m.put("cost", 10) catch unreachable;
        m.put("qty", @as(i32, @intCast(2))) catch unreachable;
        break :blk4 m;
    }, blk5: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("part", @as(i32, @intCast(100))) catch unreachable;
        m.put("supplier", @as(i32, @intCast(2))) catch unreachable;
        m.put("cost", 20) catch unreachable;
        m.put("qty", @as(i32, @intCast(1))) catch unreachable;
        break :blk5 m;
    }, blk6: {
        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
        m.put("part", @as(i32, @intCast(200))) catch unreachable;
        m.put("supplier", @as(i32, @intCast(1))) catch unreachable;
        m.put("cost", 5) catch unreachable;
        m.put("qty", @as(i32, @intCast(3))) catch unreachable;
        break :blk6 m;
    } };
    filtered = blk8: {
        var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (partsupp) |ps| {
            for (suppliers) |s| {
                if (!((s.id == ps.supplier))) continue;
                for (nations) |n| {
                    if (!((n.id == s.nation))) continue;
                    if (!(std.mem.eql(u8, n.name, "A"))) continue;
                    _tmp0.append(blk7: {
                        var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                        m.put("part", ps.part) catch unreachable;
                        m.put("value", (ps.cost * ps.qty)) catch unreachable;
                        break :blk7 m;
                    }) catch unreachable;
                }
            }
        }
        const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
        break :blk8 _tmp1;
    };
    grouped = blk11: {
        var _tmp4 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator);
        var _tmp5 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator);
        for (filtered) |x| {
            const _tmp6 = x.part;
            if (_tmp5.get(_tmp6)) |idx| {
                _tmp4.items[idx].Items.append(x) catch unreachable;
            } else {
                var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp6, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) };
                g.Items.append(x) catch unreachable;
                _tmp4.append(g) catch unreachable;
                _tmp5.put(_tmp6, _tmp4.items.len - 1) catch unreachable;
            }
        }
        var _tmp7 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (_tmp4.items) |g| {
            _tmp7.append(blk9: {
                var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                m.put("part", g.key) catch unreachable;
                m.put("total", _sum_int(blk10: {
                    var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator);
                    for (g) |r| {
                        _tmp2.append(r.value) catch unreachable;
                    }
                    const _tmp3 = _tmp2.toOwnedSlice() catch unreachable;
                    break :blk10 _tmp3;
                })) catch unreachable;
                break :blk9 m;
            }) catch unreachable;
        }
        break :blk11 _tmp7.toOwnedSlice() catch unreachable;
    };
    std.debug.print("{any}\n", .{grouped});
}
