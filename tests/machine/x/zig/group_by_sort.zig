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

var items: []const std.AutoHashMap([]const u8, i32) = undefined;
var grouped: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    items = &[_]std.AutoHashMap([]const u8, i32){ blk0: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("cat", "a") catch unreachable;
        m.put("val", @as(i32, @intCast(3))) catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("cat", "a") catch unreachable;
        m.put("val", @as(i32, @intCast(1))) catch unreachable;
        break :blk1 m;
    }, blk2: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("cat", "b") catch unreachable;
        m.put("val", @as(i32, @intCast(5))) catch unreachable;
        break :blk2 m;
    }, blk3: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("cat", "b") catch unreachable;
        m.put("val", @as(i32, @intCast(2))) catch unreachable;
        break :blk3 m;
    } };
    grouped = blk6: {
        var _tmp2 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator);
        var _tmp3 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator);
        for (items) |i| {
            const _tmp4 = i.cat;
            if (_tmp3.get(_tmp4)) |idx| {
                _tmp2.items[idx].Items.append(i) catch unreachable;
            } else {
                var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp4, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) };
                g.Items.append(i) catch unreachable;
                _tmp2.append(g) catch unreachable;
                _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable;
            }
        }
        var _tmp5 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (_tmp2.items) |g| {
            _tmp5.append(blk4: {
                var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                m.put("cat", g.key) catch unreachable;
                m.put("total", _sum_int(blk5: {
                    var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator);
                    for (g) |x| {
                        _tmp0.append(x.val) catch unreachable;
                    }
                    const _tmp1 = _tmp0.toOwnedSlice() catch unreachable;
                    break :blk5 _tmp1;
                })) catch unreachable;
                break :blk4 m;
            }) catch unreachable;
        }
        break :blk6 _tmp5.toOwnedSlice() catch unreachable;
    };
    std.debug.print("{any}\n", .{grouped});
}
