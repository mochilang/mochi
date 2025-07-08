const std = @import("std");

var people: []const std.AutoHashMap([]const u8, i32) = undefined;
var adults: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    people = &[_]std.AutoHashMap([]const u8, i32){blk0: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("name", "Alice") catch unreachable; m.put("age", 30) catch unreachable; break :blk0 m; }, blk1: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("name", "Bob") catch unreachable; m.put("age", 15) catch unreachable; break :blk1 m; }, blk2: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("name", "Charlie") catch unreachable; m.put("age", 65) catch unreachable; break :blk2 m; }, blk3: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put("name", "Diana") catch unreachable; m.put("age", 45) catch unreachable; break :blk3 m; }};
    adults = blk5: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (people) |person| { if (!((person.age >= 18))) continue; _tmp0.append(blk4: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put("name", person.name) catch unreachable; m.put("age", person.age) catch unreachable; m.put("is_senior", (person.age >= 60)) catch unreachable; break :blk4 m; }) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk5 _tmp1; };
    std.debug.print("{s}\n", .{"--- Adults ---"});
    for (adults) |person| {
        std.debug.print("{any} {s} {any} {any}\n", .{person.name, "is", person.age, if (person.is_senior) (" (senior)") else ("")});
    }
}
