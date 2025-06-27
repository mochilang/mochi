const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn romanToInt(s: []const u8) i32 {
    const values: std.AutoHashMap([]const u8, i32) = blk: {
        var m = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
        m.put("I", @as(i32, @intCast(1))) catch unreachable;
        m.put("V", @as(i32, @intCast(5))) catch unreachable;
        m.put("X", @as(i32, @intCast(10))) catch unreachable;
        m.put("L", @as(i32, @intCast(50))) catch unreachable;
        m.put("C", @as(i32, @intCast(100))) catch unreachable;
        m.put("D", @as(i32, @intCast(500))) catch unreachable;
        m.put("M", @as(i32, @intCast(1000))) catch unreachable;
        break :blk m;
    };
    var total: i32 = @as(i32, @intCast(0));
    var i: i32 = @as(i32, @intCast(0));
    const n: i32 = (s).len;
    while ((i < n)) {
        const curr: i32 = values[s[i]];
        if (((i + @as(i32, @intCast(1))) < n)) {
            const next: i32 = values[s[(i + @as(i32, @intCast(1)))]];
            if ((curr < next)) {
                total = ((total + next) - curr);
                i = (i + @as(i32, @intCast(2)));
                continue;
            }
        }
        total = (total + curr);
        i = (i + @as(i32, @intCast(1)));
    }
    return total;
}

fn test_example_1() void {
    expect((romanToInt("III") == @as(i32, @intCast(3))));
}

fn test_example_2() void {
    expect((romanToInt("LVIII") == @as(i32, @intCast(58))));
}

fn test_example_3() void {
    expect((romanToInt("MCMXCIV") == @as(i32, @intCast(1994))));
}

fn test_subtractive() void {
    expect((romanToInt("IV") == @as(i32, @intCast(4))));
    expect((romanToInt("IX") == @as(i32, @intCast(9))));
}

fn test_tens() void {
    expect((romanToInt("XL") == @as(i32, @intCast(40))));
    expect((romanToInt("XC") == @as(i32, @intCast(90))));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_subtractive();
    test_tens();
}
