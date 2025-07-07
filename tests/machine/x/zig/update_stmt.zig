const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

const Person = struct {
    name: []const u8,
    age: i32,
    status: []const u8,
};

var people: []const i32 = undefined;

fn test_update_adult_status() void {
    expect((people == &[_]i32{ Person{ .name = "Alice", .age = @as(i32, @intCast(17)), .status = "minor" }, Person{ .name = "Bob", .age = @as(i32, @intCast(26)), .status = "adult" }, Person{ .name = "Charlie", .age = @as(i32, @intCast(19)), .status = "adult" }, Person{ .name = "Diana", .age = @as(i32, @intCast(16)), .status = "minor" } }));
}

pub fn main() void {
    people = &[_]i32{ Person{ .name = "Alice", .age = @as(i32, @intCast(17)), .status = "minor" }, Person{ .name = "Bob", .age = @as(i32, @intCast(25)), .status = "unknown" }, Person{ .name = "Charlie", .age = @as(i32, @intCast(18)), .status = "unknown" }, Person{ .name = "Diana", .age = @as(i32, @intCast(16)), .status = "minor" } };
    for (0..people.len) |_tmp0| {
        var _tmp1: i32 = people[_tmp0];
        var name: u8 = _tmp1.name;
        var age: i32 = _tmp1.age;
        var status: u8 = _tmp1.status;
        var name: u8 = _tmp1.name;
        var age: i32 = _tmp1.age;
        var status: u8 = _tmp1.status;
        if ((age >= @as(i32, @intCast(18)))) {
            _tmp1.status = "adult";
            _tmp1.age = (age + @as(i32, @intCast(1)));
        }
        people[_tmp0] = _tmp1;
    }
    std.debug.print("{s}\n", .{"ok"});
    test_update_adult_status();
}
