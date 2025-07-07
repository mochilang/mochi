const std = @import("std");

fn _read_input(path: ?[]const u8) []const u8 {
    const alloc = std.heap.page_allocator;
    if (path == null or std.mem.eql(u8, path.?, "-")) {
        return std.io.getStdIn().readAllAlloc(alloc, 1 << 20) catch unreachable;
    } else {
        return std.fs.cwd().readFileAlloc(alloc, path.?, 1 << 20) catch unreachable;
    }
}

fn _write_output(path: ?[]const u8, data: []const u8) void {
    if (path == null or std.mem.eql(u8, path.?, "-")) {
        std.io.getStdOut().writeAll(data) catch unreachable;
    } else {
        std.fs.cwd().writeFile(path.?, data) catch unreachable;
    }
}

fn _save_json(rows: anytype, path: ?[]const u8) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    if (rows.len == 1) {
        std.json.stringify(rows[0], .{}, buf.writer()) catch unreachable;
    } else {
        std.json.stringify(rows, .{}, buf.writer()) catch unreachable;
    }
    _write_output(path, buf.items);
}

var people: []const std.AutoHashMap([]const u8, i32) = undefined;

pub fn main() void {
    people = &[_]std.AutoHashMap([]const u8, i32){ blk0: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Alice") catch unreachable;
        m.put("age", @as(i32, @intCast(30))) catch unreachable;
        break :blk0 m;
    }, blk1: {
        var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator);
        m.put("name", "Bob") catch unreachable;
        m.put("age", @as(i32, @intCast(25))) catch unreachable;
        break :blk1 m;
    } };
    _save_json(people, "-");
}
