// Generated by Mochi compiler v0.10.27 on 2025-07-17T17:59:22Z
const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch |err| handleError(err);
    std.debug.print("{s}\n", .{buf.items});
}

fn _equal(a: anytype, b: anytype) bool {
    if (@TypeOf(a) != @TypeOf(b)) return false;
    return std.meta.eql(a, b);
}

const PeopleItem = struct {
    name: []const u8,
    city: []const u8,
};
const people = &[_]PeopleItem{
    PeopleItem{
    .name = "Alice",
    .city = "Paris",
},
    PeopleItem{
    .name = "Bob",
    .city = "Hanoi",
},
    PeopleItem{
    .name = "Charlie",
    .city = "Paris",
},
    PeopleItem{
    .name = "Diana",
    .city = "Hanoi",
},
    PeopleItem{
    .name = "Eve",
    .city = "Paris",
},
    PeopleItem{
    .name = "Frank",
    .city = "Hanoi",
},
    PeopleItem{
    .name = "George",
    .city = "Paris",
},
}; // []const PeopleItem
const BigItem = struct {
    city: []const u8,
    num: i32,
};
const ResultStruct1 = struct { key: []const u8, Items: std.ArrayList(PeopleItem) };
var big: []const BigItem = undefined; // []const BigItem

pub fn main() void {
    big = blk0: { var _tmp2 = std.ArrayList(ResultStruct1).init(std.heap.page_allocator); for (people) |p| { const _tmp3 = p.city; var _found = false; var _idx: usize = 0; for (_tmp2.items, 0..) |it, i| { if (_equal(it.key, _tmp3)) { _found = true; _idx = i; break; } } if (_found) { _tmp2.items[_idx].Items.append(p) catch |err| handleError(err); } else { var g = ResultStruct1{ .key = _tmp3, .Items = std.ArrayList(PeopleItem).init(std.heap.page_allocator) }; g.Items.append(p) catch |err| handleError(err); _tmp2.append(g) catch |err| handleError(err); } } var _tmp4 = std.ArrayList(ResultStruct1).init(std.heap.page_allocator);for (_tmp2.items) |g| { if (!((@as(i32, @intCast(g.Items.items.len)) >= 4))) continue; _tmp4.append(g) catch |err| handleError(err); } var _tmp5 = std.ArrayList(BigItem).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp5.append(BigItem{
    .city = g.key,
    .num = @as(i32, @intCast(g.Items.items.len)),
}) catch |err| handleError(err); } const _tmp5Slice = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp5Slice; };
    _json(big);
}
