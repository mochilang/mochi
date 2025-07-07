const std = @import("std");

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var data: []const std.AutoHashMap([]const u8, i32) = undefined;
var groups: []const i32 = undefined;
var tmp: []const i32 = undefined;
var result: []const i32 = undefined;

pub fn main() void {
    data = &[_]std.AutoHashMap([]const u8, i32){ blk0: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("tag", "a") catch unreachable;
        m.put("val", @as(i32, @intCast(1))) catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("tag", "a") catch unreachable;
        m.put("val", @as(i32, @intCast(2))) catch unreachable;
        break :blk1 m;
    }, blk2: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("tag", "b") catch unreachable;
        m.put("val", @as(i32, @intCast(3))) catch unreachable;
        break :blk2 m;
    } };
    groups = blk3: {
        var _tmp0 = std.ArrayList(struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }).init(std.heap.page_allocator);
        var _tmp1 = std.AutoHashMap(i32, usize).init(std.heap.page_allocator);
        for (data) |d| {
            const _tmp2 = d.tag;
            if (_tmp1.get(_tmp2)) |idx| {
                _tmp0.items[idx].Items.append(d) catch unreachable;
            } else {
                var g = struct { key: i32, Items: std.ArrayList(std.AutoHashMap([]const u8, i32)) }{ .key = _tmp2, .Items = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator) };
                g.Items.append(d) catch unreachable;
                _tmp0.append(g) catch unreachable;
                _tmp1.put(_tmp2, _tmp0.items.len - 1) catch unreachable;
            }
        }
        var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator);
        for (_tmp0.items) |g| {
            _tmp3.append(g) catch unreachable;
        }
        break :blk3 _tmp3.toOwnedSlice() catch unreachable;
    };
    tmp = &[_]i32{};
    for (groups) |g| {
        var total = @as(i32, @intCast(0));
        for (g.items) |x| {
            total = (total + x.val);
        }
        tmp = append(tmp, blk4: {
            var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
            m.put("tag", g.key) catch unreachable;
            m.put(total, total) catch unreachable;
            break :blk4 m;
        });
    }
    result = blk5: {
        var _tmp4 = std.ArrayList(struct { item: i32, key: i32 }).init(std.heap.page_allocator);
        for (tmp) |r| {
            _tmp4.append(.{ .item = r, .key = r.tag }) catch unreachable;
        }
        for (0.._tmp4.items.len) |i| {
            for (i + 1.._tmp4.items.len) |j| {
                if (_tmp4.items[j].key < _tmp4.items[i].key) {
                    const t = _tmp4.items[i];
                    _tmp4.items[i] = _tmp4.items[j];
                    _tmp4.items[j] = t;
                }
            }
        }
        var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator);
        for (_tmp4.items) |p| {
            _tmp5.append(p.item) catch unreachable;
        }
        const _tmp6 = _tmp5.toOwnedSlice() catch unreachable;
        break :blk5 _tmp6;
    };
    std.debug.print("{any}\n", .{result});
}
