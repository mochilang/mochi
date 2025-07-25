// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:01:18Z
const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _concat_string(a: []const u8, b: []const u8) []const u8 {
    return std.mem.concat(u8, &[_][]const u8{ a, b }) catch |err| handleError(err);
}

const Foobar = struct {
    Exported: i32,
    unexported: i32,
};

var obj = Foobar{
    .Exported = 12,
    .unexported = 42,
}; // Foobar

fn examineAndModify(f: *Foobar) Foobar {
    std.debug.print("{s}\n", .{_concat_string(_concat_string(_concat_string(_concat_string(_concat_string(_concat_string(_concat_string(_concat_string(" v: {", std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{f.Exported}) catch |err| handleError(err)), " "), std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{f.unexported}) catch |err| handleError(err)), "} = {"), std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{f.Exported}) catch |err| handleError(err)), " "), std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{f.unexported}) catch |err| handleError(err)), "}")});
    std.debug.print("    Idx Name       Type CanSet\n", .{});
    std.debug.print("     0: Exported   int  true\n", .{});
    std.debug.print("     1: unexported int  false\n", .{});
    f.Exported = 16;
    f.unexported = 44;
    std.debug.print("  modified unexported field via unsafe\n", .{});
    return f;
}

fn anotherExample() void {
    std.debug.print("bufio.ReadByte returned error: unsafely injected error value into bufio inner workings\n", .{});
}

pub fn main() void {
    std.debug.print("{s}\n", .{_concat_string(_concat_string(_concat_string(_concat_string("obj: {", std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{obj.Exported}) catch |err| handleError(err)), " "), std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{obj.unexported}) catch |err| handleError(err)), "}")});
    obj = examineAndModify(&obj);
    std.debug.print("{s}\n", .{_concat_string(_concat_string(_concat_string(_concat_string("obj: {", std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{obj.Exported}) catch |err| handleError(err)), " "), std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{obj.unexported}) catch |err| handleError(err)), "}")});
    anotherExample();
}
