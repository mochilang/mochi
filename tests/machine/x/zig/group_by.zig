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

const people = (blk0: { const _tmp0 = struct {
    name: []const u8,
    age: i32,
    city: []const u8,
}; const _arr = &[_]_tmp0{
    _tmp0{
    .name = "Alice",
    .age = 30,
    .city = "Paris",
},
    _tmp0{
    .name = "Bob",
    .age = 15,
    .city = "Hanoi",
},
    _tmp0{
    .name = "Charlie",
    .age = 65,
    .city = "Paris",
},
    _tmp0{
    .name = "Diana",
    .age = 45,
    .city = "Hanoi",
},
    _tmp0{
    .name = "Eve",
    .age = 70,
    .city = "Paris",
},
    _tmp0{
    .name = "Frank",
    .age = 22,
    .city = "Hanoi",
},
}; break :blk0 _arr; });
const stats = blk2: { var _tmp3 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct {
    name: []const u8,
    age: i32,
    city: []const u8,
}) }).init(std.heap.page_allocator); var _tmp4 = std.StringHashMap(usize).init(std.heap.page_allocator); for (people) |person| { const _tmp5 = person.city; if (_tmp4.get(_tmp5)) |idx| { _tmp3.items[idx].Items.append(person) catch unreachable; } else { var g = struct { key: []const u8, Items: std.ArrayList(struct {
    name: []const u8,
    age: i32,
    city: []const u8,
}) }{ .key = _tmp5, .Items = std.ArrayList(struct {
    name: []const u8,
    age: i32,
    city: []const u8,
}).init(std.heap.page_allocator) }; g.Items.append(person) catch unreachable; _tmp3.append(g) catch unreachable; _tmp4.put(_tmp5, _tmp3.items.len - 1) catch unreachable; } } var _tmp6 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(struct {
    name: []const u8,
    age: i32,
    city: []const u8,
}) }).init(std.heap.page_allocator);for (_tmp3.items) |g| { _tmp6.append(g) catch unreachable; } var _tmp7 = std.ArrayList(struct {
    city: i32,
    count: i32,
    avg_age: f64,
}).init(std.heap.page_allocator);for (_tmp6.items) |g| { _tmp7.append(struct {
    city: i32,
    count: i32,
    avg_age: f64,
}{
    .city = g.key,
    .count = (g.Items.len),
    .avg_age = _avg_int(blk1: { var _tmp1 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |p| { _tmp1.append(p.age) catch unreachable; } const _tmp2 = _tmp1.toOwnedSlice() catch unreachable; break :blk1 _tmp2; }),
}) catch unreachable; } const _tmp7Slice = _tmp7.toOwnedSlice() catch unreachable; break :blk2 _tmp7Slice; };

pub fn main() void {
    std.debug.print("--- People grouped by city ---\n", .{});
    for (stats) |s| {
        std.debug.print("{any} {s} {any} {s} {any}\n", .{s.city, ": count =", s.count, ", avg_age =", s.avg_age});
    }
}
