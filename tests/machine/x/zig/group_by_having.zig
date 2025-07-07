const std = @import("std");

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
}

var people: []const std.AutoHashMap([]const u8, []const u8) = undefined;
var big: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    people = &[_]std.AutoHashMap([]const u8, []const u8){ blk0: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Alice") catch unreachable;
        m.put("city", "Paris") catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Bob") catch unreachable;
        m.put("city", "Hanoi") catch unreachable;
        break :blk1 m;
    }, blk2: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Charlie") catch unreachable;
        m.put("city", "Paris") catch unreachable;
        break :blk2 m;
    }, blk3: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Diana") catch unreachable;
        m.put("city", "Hanoi") catch unreachable;
        break :blk3 m;
    }, blk4: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Eve") catch unreachable;
        m.put("city", "Paris") catch unreachable;
        break :blk4 m;
    }, blk5: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Frank") catch unreachable;
        m.put("city", "Hanoi") catch unreachable;
        break :blk5 m;
    }, blk6: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "George") catch unreachable;
        m.put("city", "Paris") catch unreachable;
        break :blk6 m;
    } };
    big = blk8: {
        var _tmp0 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(std.AutoHashMap([]const u8, []const u8)) }).init(std.heap.page_allocator);
        var _tmp1 = std.AutoHashMap([]const u8, usize).init(std.heap.page_allocator);
        for (people) |p| {
            const _tmp2 = p.city;
            if (_tmp1.get(_tmp2)) |idx| {
                _tmp0.items[idx].Items.append(p) catch unreachable;
            } else {
                var g = struct { key: []const u8, Items: std.ArrayList(std.AutoHashMap([]const u8, []const u8)) }{ .key = _tmp2, .Items = std.ArrayList(std.AutoHashMap([]const u8, []const u8)).init(std.heap.page_allocator) };
                g.Items.append(p) catch unreachable;
                _tmp0.append(g) catch unreachable;
                _tmp1.put(_tmp2, _tmp0.items.len - 1) catch unreachable;
            }
        }
        var _tmp3 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator);
        for (_tmp0.items) |g| {
            _tmp3.append(blk7: {
                var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator);
                m.put("city", g.key) catch unreachable;
                m.put("num", (g.Items.len)) catch unreachable;
                break :blk7 m;
            }) catch unreachable;
        }
        break :blk8 _tmp3.toOwnedSlice() catch unreachable;
    };
    _json(big);
}
