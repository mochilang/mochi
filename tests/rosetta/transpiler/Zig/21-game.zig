// Generated by Mochi Zig transpiler on 2025-07-27 00:44 +0700
const std = @import("std");

const Map0 = struct {
    0: i64,
    1: i64,
    2: i64,
    3: i64,
    4: i64,
    5: i64,
    6: i64,
    7: i64,
    8: i64,
    9: i64,
};

fn parseIntStr(str: []const u8) i64 {
    var i: i64 = 0;
    var neg: bool = false;
    if (std.mem.len(str) > 0 and std.mem.eql(u8, str[0..1], "-")) {
        neg = true;
        i = 1;
    }
    var n: i64 = 0;
    const digits: Map0 = .{ .0 = 0, .1 = 1, .2 = 2, .3 = 3, .4 = 4, .5 = 5, .6 = 6, .7 = 7, .8 = 8, .9 = 9 };
    while (i < std.mem.len(str)) {
        n = n * 10 + digits[@as(usize, @intCast(str[i..i + 1]))];
        i = i + 1;
    }
    if (neg) {
        n = 0 - n;
    }
    return n;
}

fn mochi_main() void {
    var total: i64 = 0;
    var computer: bool = @mod(_now(), 2) == 0;
    std.debug.print("{s}\n", .{"Enter q to quit at any time\n"});
    if (computer) {
        std.debug.print("{s}\n", .{"The computer will choose first"});
    } else {
        std.debug.print("{s}\n", .{"You will choose first"});
    }
    std.debug.print("{s}\n", .{"\n\nRunning total is now 0\n\n"});
    var round: i64 = 1;
    var done: bool = false;
    while (!(done)) {
        std.debug.print("{s}\n", .{_concat_string(_concat_string("ROUND ", _str(round)), ":\n\n")});
        var i: i64 = 0;
        while (i < 2 and !(done)) {
            if (computer) {
                var choice: i64 = 0;
                if (total < 18) {
                    choice = @mod(_now(), 3) + 1;
                } else {
                    choice = 21 - total;
                }
                total = total + choice;
                std.debug.print("{s}\n", .{_concat_string("The computer chooses ", _str(choice))});
                std.debug.print("{s}\n", .{_concat_string("Running total is now ", _str(total))});
                if (total == 21) {
                    std.debug.print("{s}\n", .{"\nSo, commiserations, the computer has won!"});
                    done = true;
                }
            } else {
                while (true) {
                    std.debug.print("{s}\n", .{"Your choice 1 to 3 : "});
                    const line: []const u8 = _input();
                    if (std.mem.eql(u8, line, "q") or std.mem.eql(u8, line, "Q")) {
                        std.debug.print("{s}\n", .{"OK, quitting the game"});
                        done = true;
                        break;
                    }
                    var num: i64 = parseIntStr(line);
                    if (num < 1 or num > 3) {
                        if (total + num > 21) {
                            std.debug.print("{s}\n", .{"Too big, try again"});
                        } else {
                            std.debug.print("{s}\n", .{"Out of range, try again"});
                        }
                        continue;
                    }
                    if (total + num > 21) {
                        std.debug.print("{s}\n", .{"Too big, try again"});
                        continue;
                    }
                    total = total + num;
                    std.debug.print("{s}\n", .{_concat_string("Running total is now ", _str(total))});
                    break;
                }
                if (total == 21) {
                    std.debug.print("{s}\n", .{"\nSo, congratulations, you've won!"});
                    done = true;
                }
            }
            std.debug.print("{s}\n", .{"\n"});
            computer = !(computer);
            i = i + 1;
        }
        round = round + 1;
    }
}

pub fn main() void {
    {
        const __start_mem = _mem();
        const __start = _now();
        mochi_main();
        const __end = _now();
        const __end_mem = _mem();
        const __duration_us = @divTrunc(@as(i64, @intCast(__end - __start)), 1000);
        const __memory_bytes = __end_mem - __start_mem;
        const __bench = .{ .duration_us = __duration_us, .memory_bytes = __memory_bytes, .name = "main" };
        const __bj = std.fmt.allocPrint(std.heap.page_allocator, "{f}", .{std.json.fmt(__bench, .{ .whitespace = .indent_2 })}) catch unreachable;
        std.debug.print("{s}\n", .{__bj});
        std.heap.page_allocator.free(__bj);
    }
}

var _now_seed: i64 = 0;
var _now_seeded: bool = false;
fn _now() i64 {
    if (_now_seeded) {
        _now_seed = @mod(_now_seed * 1664525 + 1013904223, 2147483647);
        return _now_seed;
    }
    if (! _now_seeded) {
        if (std.process.getEnvVarOwned(std.heap.page_allocator, "MOCHI_NOW_SEED")) |s| {
            defer std.heap.page_allocator.free(s);
            if (std.fmt.parseInt(i64, s, 10)) |v| {
                _now_seed = v;
                _now_seeded = true;
                _now_seed = @mod(_now_seed * 1664525 + 1013904223, 2147483647);
                return _now_seed;
            } else |_| {}
        } else |_| {}
    }
    return @as(i64, @intCast(std.time.nanoTimestamp()));
}

fn _str(v: anytype) []const u8 {
    if (@TypeOf(v) == f64 or @TypeOf(v) == f32) {
        return std.fmt.allocPrint(std.heap.page_allocator, "{d:.1}", .{v}) catch unreachable;
    }
    return std.fmt.allocPrint(std.heap.page_allocator, "{any}", .{v}) catch unreachable;
}

fn _concat_string(a: []const u8, b: []const u8) []const u8 {
    const alloc = std.heap.page_allocator;
    return std.mem.concat(alloc, u8, &[_][]const u8{ a, b }) catch unreachable;
}

var _in_buf = std.io.bufferedReader(std.io.getStdIn().reader());
fn _input() []const u8 {
    const opt_line = _in_buf.reader().readUntilDelimiterOrEofAlloc(std.heap.page_allocator, '\n', 1 << 20) catch return "";
    const line = opt_line orelse return "";
    if (line.len > 0 and line[line.len - 1] == '\n') {
        return line[0..line.len-1];
    }
    return line;
}

fn _mem() i64 {
    var usage: std.os.linux.rusage = undefined;
    if (std.os.linux.getrusage(std.os.linux.rusage.SELF, &usage) != 0) return 0;
    return @as(i64, usage.maxrss) * 1024;
}
