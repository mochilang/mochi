const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn isMatch(s: []const u8, p: []const u8) bool {
    const m: i32 = (s).len;
    const n: i32 = (p).len;
    var dp = std.ArrayList(bool).init(std.heap.page_allocator);
    var i: i32 = @as(i32, @intCast(0));
    while ((i <= m)) {
        var row = std.ArrayList(bool).init(std.heap.page_allocator);
        var j: i32 = @as(i32, @intCast(0));
        while ((j <= n)) {
            try row.append(@as(i32, @intCast(false)));
            j = (j + @as(i32, @intCast(1)));
        }
        try dp.append(@as(i32, @intCast(row)));
        i = (i + @as(i32, @intCast(1)));
    }
    dp.items[m][n] = true;
    var _i2: i32 = m;
    while ((_i2 >= @as(i32, @intCast(0)))) {
        var j2: i32 = (n - @as(i32, @intCast(1)));
        while ((j2 >= @as(i32, @intCast(0)))) {
            var first: bool = false;
            if ((_i2 < m)) {
                if ((((p[j2] == s[_i2])) or (std.mem.eql(u8, p[j2], ".")))) {
                    first = true;
                }
            }
            var star: bool = false;
            if (((j2 + @as(i32, @intCast(1))) < n)) {
                if (std.mem.eql(u8, p[(j2 + @as(i32, @intCast(1)))], "*")) {
                    star = true;
                }
            }
            if (star) {
                var ok: bool = false;
                if (dp[_i2][(j2 + @as(i32, @intCast(2)))]) {
                    ok = true;
                } else {
                    if (first) {
                        if (dp[(_i2 + @as(i32, @intCast(1)))][j2]) {
                            ok = true;
                        }
                    }
                }
                dp.items[_i2][j2] = ok;
            } else {
                var ok: bool = false;
                if (first) {
                    if (dp[(_i2 + @as(i32, @intCast(1)))][(j2 + @as(i32, @intCast(1)))]) {
                        ok = true;
                    }
                }
                dp.items[_i2][j2] = ok;
            }
            j2 = (j2 - @as(i32, @intCast(1)));
        }
        _i2 = (_i2 - @as(i32, @intCast(1)));
    }
    return dp.items[@as(i32, @intCast(0))][@as(i32, @intCast(0))];
}

fn test_example_1() void {
    expect((isMatch("aa", "a") == false));
}

fn test_example_2() void {
    expect((isMatch("aa", "a*") == true));
}

fn test_example_3() void {
    expect((isMatch("ab", ".*") == true));
}

fn test_example_4() void {
    expect((isMatch("aab", "c*a*b") == true));
}

fn test_example_5() void {
    expect((isMatch("mississippi", "mis*is*p*.") == false));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_example_4();
    test_example_5();
}
