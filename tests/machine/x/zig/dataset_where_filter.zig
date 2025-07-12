const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

const PeopleItem = struct {
    name: []const u8,
    age: i32,
};
const people = &[_]PeopleItem{
    PeopleItem{
    .name = "Alice",
    .age = 30,
},
    PeopleItem{
    .name = "Bob",
    .age = 15,
},
    PeopleItem{
    .name = "Charlie",
    .age = 65,
},
    PeopleItem{
    .name = "Diana",
    .age = 45,
},
}; // []const PeopleItem
const adults = blk0: { var _tmp0 = std.ArrayList(struct {
    name: []const u8,
    age: i32,
    is_senior: bool,
}).init(std.heap.page_allocator); for (people) |person| { if (!((person.age >= 18))) continue; _tmp0.append(struct {
    name: []const u8,
    age: i32,
    is_senior: bool,
}{
    .name = person.name,
    .age = person.age,
    .is_senior = (person.age >= 60),
}) catch |err| handleError(err); } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; }; // []const std.StringHashMap(i32)

pub fn main() void {
    std.debug.print("--- Adults ---\n", .{});
    for (adults) |person| {
        std.debug.print("{any} {s} {any} {s}\n", .{person.name, "is", person.age, if (person.is_senior) (" (senior)") else ("")});
    }
}
