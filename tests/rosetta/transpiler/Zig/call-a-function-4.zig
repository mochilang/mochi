// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:01:21Z
const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn gifEncode(out: i32, img: i32, opts: std.AutoHashMap([]const u8, i32)) void {
}

fn user_main() void {
    var opts = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
    _ = opts.put("NumColors", 16) catch |err| handleError(err);
    gifEncode(0, 0, opts);
}

pub fn main() void {
    user_main();
}
