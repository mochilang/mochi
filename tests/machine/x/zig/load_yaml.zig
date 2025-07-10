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

fn _load_json(comptime T: type, path: ?[]const u8) []T {
    const text = _read_input(path);
    return std.json.parseFromSlice(T, std.heap.page_allocator, text, .{}).value;
}

const Person = struct {
    name: []const u8,
    age: i32,
    email: []const u8,
};

const people = _load_json([]Person, "../interpreter/valid/people.yaml");
const adults = blk0: { var _tmp0 = std.ArrayList(struct { name: []const u8, email: []const u8, }).init(std.heap.page_allocator); for (people) |p| { if (!((p.age >= 18))) continue; _tmp0.append(struct { name: []const u8, email: []const u8, }{ .name = p.name, .email = p.email }) catch unreachable; } const _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk0 _tmp1; };

pub fn main() void {
    for (adults) |a| {
        std.debug.print("{s} {s}\n", .{a.name, a.email});
    }
}
