const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.page_allocator;
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);
    if (args.len != 2) {
        std.debug.print("usage: {s} <file>\n", .{args[0]});
        return;
    }
    const path = args[1];
    const bytes = try std.fs.cwd().readFileAlloc(gpa, path, 1<<20);
    defer gpa.free(bytes);
    const sentinel = try gpa.alloc(u8, bytes.len + 1);
    defer gpa.free(sentinel);
    std.mem.copyForwards(u8, sentinel[0..bytes.len], bytes);
    sentinel[bytes.len] = 0;
    const slice = sentinel[0..bytes.len :0];
    var ast = try std.zig.Ast.parse(gpa, slice, .zig);
    defer ast.deinit(gpa);

    var wr = std.io.getStdOut().writer();
    try wr.writeAll("{\"nodes\":[");
    var first = true;
    for (ast.nodes.items(.tag), ast.nodes.items(.main_token), ast.nodes.items(.data)) |tag, main_tok, data| {
        if (!first) {
            try wr.writeByte(',');
        } else {
            first = false;
        }
        try std.json.stringify(.{ .tag = @intFromEnum(tag), .main = main_tok, .lhs = data.lhs, .rhs = data.rhs }, .{}, wr);
    }
    try wr.writeAll("]}");
}
