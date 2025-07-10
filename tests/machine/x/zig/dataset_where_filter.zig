const std = @import("std");

const people = (blk0: { const _tmp0 = struct {
    name: []const u8,
    age: i32,
}; const _arr = &[_]_tmp0{
    _tmp0{
    .name = "Alice",
    .age = 30,
},
    _tmp0{
    .name = "Bob",
    .age = 15,
},
    _tmp0{
    .name = "Charlie",
    .age = 65,
},
    _tmp0{
    .name = "Diana",
    .age = 45,
},
}; break :blk0 _arr; });
const adults = blk1: { var _tmp1 = std.ArrayList(struct {
    name: []const u8,
    age: i32,
    is_senior: bool,
}).init(std.heap.page_allocator); for (people) |person| { if (!((person.age >= 18))) continue; _tmp1.append(struct {
    name: []const u8,
    age: i32,
    is_senior: bool,
}{
    .name = person.name,
    .age = person.age,
    .is_senior = (person.age >= 60),
}) catch unreachable; } const _tmp2 = _tmp1.toOwnedSlice() catch unreachable; break :blk1 _tmp2; };

pub fn main() void {
    std.debug.print("--- Adults ---\n", .{});
    for (adults) |person| {
        std.debug.print("{any} {s} {any} {s}\n", .{person.name, "is", person.age, if (person.is_senior) (" (senior)") else ("")});
    }
}
