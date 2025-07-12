const std = @import("std");

fn _avg_int(v: []const i32) i32 {
    if (v.len == 0) return 0;
    var sum: i32 = 0;
    for (v) |it| { sum += it; }
    return @divTrunc(sum, @as(i32, @intCast(v.len)));
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
    age: i32,
    city: []const u8,
};
const people = &[_]PeopleItem{
    PeopleItem{
    .name = "Alice",
    .age = 30,
    .city = "Paris",
},
    PeopleItem{
    .name = "Bob",
    .age = 15,
    .city = "Hanoi",
},
    PeopleItem{
    .name = "Charlie",
    .age = 65,
    .city = "Paris",
},
    PeopleItem{
    .name = "Diana",
    .age = 45,
    .city = "Hanoi",
},
    PeopleItem{
    .name = "Eve",
    .age = 70,
    .city = "Paris",
},
    PeopleItem{
    .name = "Frank",
    .age = 22,
    .city = "Hanoi",
},
}; // []const PeopleItem
const stats = blk1: { var _tmp2 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(PeopleItem) }).init(std.heap.page_allocator); var _tmp3 = std.StringHashMap(usize).init(std.heap.page_allocator); for (people) |person| { const _tmp4 = person.city; if (_tmp3.get(_tmp4)) |idx| { _tmp2.items[idx].Items.append(person) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(PeopleItem) }{ .key = _tmp4, .Items = std.ArrayList(PeopleItem).init(std.heap.page_allocator) }; g.Items.append(person) catch unreachable; _tmp2.append(g) catch unreachable; _tmp3.put(_tmp4, _tmp2.items.len - 1) catch unreachable; } } var _tmp5 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(PeopleItem) }).init(std.heap.page_allocator);for (_tmp2.items) |g| { _tmp5.append(g) catch unreachable; } var _tmp6 = std.ArrayList(struct {
    city: i32,
    count: i32,
    avg_age: f64,
}).init(std.heap.page_allocator);for (_tmp5.items) |g| { _tmp6.append(struct {
    city: i32,
    count: i32,
    avg_age: f64,
}{
    .city = g.key,
    .count = (g.Items.len),
    .avg_age = _avg_int(blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |p| { _tmp0.append(p.age) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; }),
}) catch unreachable; } const _tmp6Slice = _tmp6.toOwnedSlice() catch unreachable; break :blk1 _tmp6Slice; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- People grouped by city ---\n", .{});
    for (stats) |s| {
        std.debug.print("{any} {s} {any} {s} {any}\n", .{s.city, ": count =", s.count, ", avg_age =", s.avg_age});
    }
}
