const std = @import("std");

const Person = struct {
    name: []const u8,
    age: i32,
};

const Book = struct {
    title: []const u8,
    author: i32,
};

var book: i32 = undefined;

pub fn main() void {
    book = Book{ .title = "Go", .author = Person{ .name = "Bob", .age = @as(i32, @intCast(42)) } };
    std.debug.print("{any}\n", .{book.author.name});
}
