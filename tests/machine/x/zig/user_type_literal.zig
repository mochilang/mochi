const std = @import("std");

const Person = struct {
    name: []const u8,
    age: i32,
};

const Book = struct {
    title: []const u8,
    author: Person,
};

const book = Book{
    .title = "Go",
    .author = Person{
    .name = "Bob",
    .age = 42,
},
};

pub fn main() void {
    std.debug.print("{s}\n", .{book.author.name});
}
