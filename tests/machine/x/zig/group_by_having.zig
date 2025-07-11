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
}; // []const Peopleitem
const big = blk0: { var _tmp0 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(PeopleItem) }).init(std.heap.page_allocator); var _tmp1 = std.StringHashMap(usize).init(std.heap.page_allocator); for (people) |p| { const _tmp2 = p.city; if (_tmp1.get(_tmp2)) |idx| { _tmp0.items[idx].Items.append(p) catch |err| handleError(err); } else { var g = struct { key: []const u8, Items: std.ArrayList(PeopleItem) }{ .key = _tmp2, .Items = std.ArrayList(PeopleItem).init(std.heap.page_allocator) }; g.Items.append(p) catch |err| handleError(err); _tmp0.append(g) catch |err| handleError(err); _tmp1.put(_tmp2, _tmp0.items.len - 1) catch |err| handleError(err); } } var _tmp3 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(PeopleItem) }).init(std.heap.page_allocator);for (_tmp0.items) |g| { if (!(((g.Items.len) >= 4))) continue; _tmp3.append(g) catch |err| handleError(err); } var _tmp4 = std.ArrayList(struct {
    city: i32,
    num: i32,
}).init(std.heap.page_allocator);for (_tmp3.items) |g| { _tmp4.append(struct {
    city: i32,
    num: i32,
}{
    .city = g.key,
    .num = (g.Items.len),
}) catch |err| handleError(err); } const _tmp4Slice = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp4Slice; }; // []const std.StringHashMap(i32)

pub fn main() void {
    _json(big);
}
