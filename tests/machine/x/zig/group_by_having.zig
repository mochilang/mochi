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
    return switch (@typeInfo(@TypeOf(a))) {
        .Struct, .Union, .Array, .Vector, .Pointer, .Slice => std.meta.eql(a, b),
        else => a == b,
    };
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
const ResultStruct0 = struct {
    city: i32,
    num: i32,
};
const big = blk0: { var _tmp1 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(PeopleItem) }).init(std.heap.page_allocator); var _tmp2 = std.StringHashMap(usize).init(std.heap.page_allocator); for (people) |p| { const _tmp3 = p.city; if (_tmp2.get(_tmp3)) |idx| { _tmp1.items[idx].Items.append(p) catch |err| handleError(err); } else { var g = struct { key: []const u8, Items: std.ArrayList(PeopleItem) }{ .key = _tmp3, .Items = std.ArrayList(PeopleItem).init(std.heap.page_allocator) }; g.Items.append(p) catch |err| handleError(err); _tmp1.append(g) catch |err| handleError(err); _tmp2.put(_tmp3, _tmp1.items.len - 1) catch |err| handleError(err); } } var _tmp4 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(PeopleItem) }).init(std.heap.page_allocator);for (_tmp1.items) |g| { if (!(((g.Items.len) >= 4))) continue; _tmp4.append(g) catch |err| handleError(err); } var _tmp5 = std.ArrayList(struct {
    city: i32,
    num: i32,
}).init(std.heap.page_allocator);for (_tmp4.items) |g| { _tmp5.append(ResultStruct0{
    .city = g.key,
    .num = (g.Items.len),
}) catch |err| handleError(err); } const _tmp5Slice = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp5Slice; }; // []const std.StringHashMap(i32)

pub fn main() void {
    _json(big);
}
