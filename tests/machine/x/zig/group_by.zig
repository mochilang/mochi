const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

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
const ResultStruct0 = struct {
    city: i32,
    count: i32,
    avg_age: i32,
};
const stats = blk2: { var _tmp5 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(PeopleItem) }).init(std.heap.page_allocator); var _tmp6 = std.StringHashMap(usize).init(std.heap.page_allocator); for (people) |person| { const _tmp7 = person.city; if (_tmp6.get(_tmp7)) |idx| { _tmp5.items[idx].Items.append(person) catch |err| handleError(err); } else { var g = struct { key: []const u8, Items: std.ArrayList(PeopleItem) }{ .key = _tmp7, .Items = std.ArrayList(PeopleItem).init(std.heap.page_allocator) }; g.Items.append(person) catch |err| handleError(err); _tmp5.append(g) catch |err| handleError(err); _tmp6.put(_tmp7, _tmp5.items.len - 1) catch |err| handleError(err); } } var _tmp8 = std.ArrayList(struct { key: []const u8, Items: std.ArrayList(PeopleItem) }).init(std.heap.page_allocator);for (_tmp5.items) |g| { _tmp8.append(g) catch |err| handleError(err); } var _tmp9 = std.ArrayList(struct {
    city: i32,
    count: i32,
    avg_age: f64,
}).init(std.heap.page_allocator);for (_tmp8.items) |g| { _tmp9.append(ResultStruct0{
    .city = g.key,
    .count = (g.Items.len),
    .avg_age = _avg_int(blk1: { var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator); for (g) |p| { _tmp3.append(p.age) catch |err| handleError(err); } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; }),
}) catch |err| handleError(err); } const _tmp9Slice = _tmp9.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp9Slice; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- People grouped by city ---\n", .{});
    for (stats) |s| {
        std.debug.print("{any} {s} {any} {s} {any}\n", .{s.city, ": count =", s.count, ", avg_age =", s.avg_age});
    }
}
