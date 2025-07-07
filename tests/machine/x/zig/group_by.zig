const std = @import("std");

fn _avg_int(v: []const i32) f64 {
    if (v.len == 0) return 0;
    var sum: f64 = 0;
    for (v) |it| {
        sum += @floatFromInt(it);
    }
    return sum / @as(f64, @floatFromInt(v.len));
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var people: []const std.AutoHashMap([]const u8, i32) = undefined;
var stats: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    people = &[_]std.AutoHashMap([]const u8, i32){ blk0: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Alice") catch unreachable;
        m.put("age", @as(i32, @intCast(30))) catch unreachable;
        m.put("city", "Paris") catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Bob") catch unreachable;
        m.put("age", @as(i32, @intCast(15))) catch unreachable;
        m.put("city", "Hanoi") catch unreachable;
        break :blk1 m;
    }, blk2: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Charlie") catch unreachable;
        m.put("age", @as(i32, @intCast(65))) catch unreachable;
        m.put("city", "Paris") catch unreachable;
        break :blk2 m;
    }, blk3: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Diana") catch unreachable;
        m.put("age", @as(i32, @intCast(45))) catch unreachable;
        m.put("city", "Hanoi") catch unreachable;
        break :blk3 m;
    }, blk4: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Eve") catch unreachable;
        m.put("age", @as(i32, @intCast(70))) catch unreachable;
        m.put("city", "Paris") catch unreachable;
        break :blk4 m;
    }, blk5: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Frank") catch unreachable;
        m.put("age", @as(i32, @intCast(22))) catch unreachable;
        m.put("city", "Hanoi") catch unreachable;
        break :blk5 m;
    } };
    stats = blk8: {
        var _tmp2 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator);
        var _tmp3 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator);
        for (people) |person| {
            const _tmp4 = person.city;
            if (_tmp3.get(_tmp4)) |idx| {
                _tmp2.items[idx].Items.append(person) catch unreachable;
            } else {
                var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) };
                g.Items.append(person) catch unreachable;
                _tmp2.append(g) catch unreachable;
                _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable;
            }
        }
        var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (_tmp2.items) |g| {
            _tmp5.append(blk6: {
                var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                m.put("city", g.key) catch unreachable;
                m.put(count, (g.Items.len)) catch unreachable;
                m.put("avg_age", _avg_int(blk7: {
                    var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator);
                    for (g) |p| {
                        _tmp0.append(p.age) catch unreachable;
                    }
                    const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
                    break :blk7 _tmp1;
                })) catch unreachable;
                break :blk6 m;
            }) catch unreachable;
        }
        break :blk8 _tmp5.toOwnedSlice() catch unreachable;
    };
    std.debug.print("{s}\n", .{"--- People grouped by city ---"});
    for (stats) |s| {
        std.debug.print("{any} {s} {any} {s} {any}\n", .{ s.city, ": count =", s.count, ", avg_age =", s.avg_age });
    }
}
