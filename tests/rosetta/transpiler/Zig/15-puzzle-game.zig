// Generated by Mochi Zig transpiler on 2025-07-27 01:40 +0700
const std = @import("std");

const MoveResult = struct {
    idx: i64,
    ok: bool,
};

var board: [16]i64 = [16]i64{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0};
const solved: []i64 = @constCast(&[_]i64{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0})[0..];
var empty: i64 = 15;
var moves: i64 = 0;
var quit: bool = false;

fn randMove() i64 {
    return @mod(_now(), 4);
}

fn isSolved() bool {
    var i: i64 = 0;
    while (i < 16) {
        if (board[@as(usize, @intCast(i))] != solved[@as(usize, @intCast(i))]) {
            return false;
        }
        i = i + 1;
    }
    return true;
}

fn isValidMove(m: i64) MoveResult {
    if (m == 0) {
        return .{ .idx = empty - 4, .ok = @divTrunc(empty, 4) > 0 };
    }
    if (m == 1) {
        return .{ .idx = empty + 4, .ok = @divTrunc(empty, 4) < 3 };
    }
    if (m == 2) {
        return .{ .idx = empty + 1, .ok = @mod(empty, 4) < 3 };
    }
    if (m == 3) {
        return .{ .idx = empty - 1, .ok = @mod(empty, 4) > 0 };
    }
    return .{ .idx = 0, .ok = false };
}

fn doMove(m: i64) bool {
    const r: MoveResult = isValidMove(m);
    if (!(r.ok)) {
        return false;
    }
    const i: i64 = empty;
    const j: i64 = r.idx;
    const tmp: i64 = board[@as(usize, @intCast(i))];
    board[@as(usize, @intCast(i))] = board[@as(usize, @intCast(j))];
    board[@as(usize, @intCast(j))] = tmp;
    empty = j;
    moves = moves + 1;
    return true;
}

fn shuffle(n: i64) void {
    var i: i64 = 0;
    while (i < n or isSolved()) {
        if (doMove(randMove())) {
            i = i + 1;
        }
    }
}

fn printBoard() void {
    var line: []const u8 = "";
    var i: i64 = 0;
    while (i < 16) {
        const val: i64 = board[@as(usize, @intCast(i))];
        if (val == 0) {
            line = _concat_string(line, "  .");
        } else {
            const s: []const u8 = _str(val);
            if (val < 10) {
                line = _concat_string(_concat_string(line, "  "), s);
            } else {
                line = _concat_string(_concat_string(line, " "), s);
            }
        }
        if (@mod(i, 4) == 3) {
            std.debug.print("{s}\n", .{line});
            line = "";
        }
        i = i + 1;
    }
}

fn playOneMove() void {
    while (true) {
        std.debug.print("{s}\n", .{_concat_string(_concat_string("Enter move #", _str(moves + 1)), " (U, D, L, R, or Q): ")});
        const s: []const u8 = _input();
        if (std.mem.eql(u8, s, "")) {
            continue;
        }
        const c: []const u8 = s[0..1];
        var m: i64 = 0;
        if (std.mem.eql(u8, c, "U") or std.mem.eql(u8, c, "u")) {
            m = 0;
        } else {
            if (std.mem.eql(u8, c, "D") or std.mem.eql(u8, c, "d")) {
                m = 1;
            } else {
                if (std.mem.eql(u8, c, "R") or std.mem.eql(u8, c, "r")) {
                    m = 2;
                } else {
                    if (std.mem.eql(u8, c, "L") or std.mem.eql(u8, c, "l")) {
                        m = 3;
                    } else {
                        if (std.mem.eql(u8, c, "Q") or std.mem.eql(u8, c, "q")) {
                            std.debug.print("{s}\n", .{_concat_string(_concat_string("Quiting after ", _str(moves)), " moves.")});
                            quit = true;
                            return;
                        } else {
                            std.debug.print("{s}\n", .{_concat_string(_concat_string(_concat_string("Please enter \"U\", \"D\", \"L\", or \"R\" to move the empty cell\n", "up, down, left, or right. You can also enter \"Q\" to quit.\n"), "Upper or lowercase is accepted and only the first non-blank\n"), "character is important (i.e. you may enter \"up\" if you like).")});
                            continue;
                        }
                    }
                }
            }
        }
        if (!(doMove(m))) {
            std.debug.print("{s}\n", .{"That is not a valid move at the moment."});
            continue;
        }
        return;
    }
}

fn play() void {
    std.debug.print("{s}\n", .{"Starting board:"});
    while (!(quit) and isSolved() == false) {
        std.debug.print("{s}\n", .{""});
        printBoard();
        playOneMove();
    }
    if (isSolved()) {
        std.debug.print("{s}\n", .{_concat_string(_concat_string("You solved the puzzle in ", _str(moves)), " moves.")});
    }
}

fn mochi_main() void {
    shuffle(50);
    play();
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

fn _input() []const u8 {
    var reader = std.io.bufferedReaderSize(4096, std.io.getStdIn().reader());
    const opt_line = reader.reader().readUntilDelimiterOrEofAlloc(std.heap.page_allocator, '\n', 1 << 20) catch return "";
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
