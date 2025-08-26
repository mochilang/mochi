const std = @import("std");

// https://www.spoj.com/problems/TEST/

pub fn main() !void {
    var reader = std.io.getStdIn().reader();
    var writer = std.io.getStdOut().writer();

    var value: i32 = 0;
    var reading = false;

    while (true) {
        const b = reader.readByte() catch |err| {
            if (err == error.EndOfStream) break;
            return err;
        };
        if (b >= '0' and b <= '9') {
            value = value * 10 + @as(i32, b - '0');
            reading = true;
        } else {
            if (reading) {
                if (value == 42) return;
                try writer.print("{}\n", .{value});
                value = 0;
                reading = false;
            }
        }
    }
    if (reading and value != 42) {
        try writer.print("{}\n", .{value});
    }
}
