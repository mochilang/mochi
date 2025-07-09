const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

const Person = struct {
    name: []const u8,
    age: i32,
    status: []const u8,
};

const people = &[_]Person{Person{ .name = "Alice", .age = 17, .status = "minor" }, Person{ .name = "Bob", .age = 25, .status = "unknown" }, Person{ .name = "Charlie", .age = 18, .status = "unknown" }, Person{ .name = "Diana", .age = 16, .status = "minor" }};

fn test_update_adult_status() void {
    expect((people == &[_]Person{Person{ .name = "Alice", .age = 17, .status = "minor" }, Person{ .name = "Bob", .age = 26, .status = "adult" }, Person{ .name = "Charlie", .age = 19, .status = "adult" }, Person{ .name = "Diana", .age = 16, .status = "minor" }}));
}

pub fn main() void {
    for (0..people.len) |_tmp0| {
        var _tmp1: Person = people[_tmp0];
        var name: u8 = _tmp1.name;
        var age: i32 = _tmp1.age;
        var status: u8 = _tmp1.status;
        var name: u8 = _tmp1.name;
        var age: i32 = _tmp1.age;
        var status: u8 = _tmp1.status;
        if ((age >= 18)) {
            _tmp1.status = "adult";
            _tmp1.age = (age + 1);
        }
        people[_tmp0] = _tmp1;
    }
    std.debug.print("{s}\n", .{"ok"});
    test_update_adult_status();
}
