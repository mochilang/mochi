const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn digit(ch: []const u8) i32 {
    if (std.mem.eql(u8, ch, "0")) {
        return @as(i32,@intCast(0));
    }
    if (std.mem.eql(u8, ch, "1")) {
        return @as(i32,@intCast(1));
    }
    if (std.mem.eql(u8, ch, "2")) {
        return @as(i32,@intCast(2));
    }
    if (std.mem.eql(u8, ch, "3")) {
        return @as(i32,@intCast(3));
    }
    if (std.mem.eql(u8, ch, "4")) {
        return @as(i32,@intCast(4));
    }
    if (std.mem.eql(u8, ch, "5")) {
        return @as(i32,@intCast(5));
    }
    if (std.mem.eql(u8, ch, "6")) {
        return @as(i32,@intCast(6));
    }
    if (std.mem.eql(u8, ch, "7")) {
        return @as(i32,@intCast(7));
    }
    if (std.mem.eql(u8, ch, "8")) {
        return @as(i32,@intCast(8));
    }
    if (std.mem.eql(u8, ch, "9")) {
        return @as(i32,@intCast(9));
    }
    return -@as(i32,@intCast(1));
}

fn myAtoi(s: []const u8) i32 {
    var i: i32 = @as(i32,@intCast(0));
    const n: i32 = (s).len;
    while ((((i < n) and s[i]) == " "[@as(i32,@intCast(0))])) {
        i = (i + @as(i32,@intCast(1)));
    }
    var sign: i32 = @as(i32,@intCast(1));
    if (((i < n) and ((((s[i] == "+"[@as(i32,@intCast(0))]) or s[i]) == "-"[@as(i32,@intCast(0))])))) {
        if ((s[i] == "-"[@as(i32,@intCast(0))])) {
            sign = -@as(i32,@intCast(1));
        }
        i = (i + @as(i32,@intCast(1)));
    }
    var result: i32 = @as(i32,@intCast(0));
    while ((i < n)) {
        const ch: i32 = s[i..(i + @as(i32,@intCast(1)))];
        const d: i32 = digit(ch);
        if ((d < @as(i32,@intCast(0)))) {
            break;
        }
        result = ((result * @as(i32,@intCast(10))) + d);
        i = (i + @as(i32,@intCast(1)));
    }
    result = (result * sign);
    if ((result > @as(i32,@intCast(2147483647)))) {
        return @as(i32,@intCast(2147483647));
    }
    if ((result < (-@as(i32,@intCast(2147483648))))) {
        return -@as(i32,@intCast(2147483648));
    }
    return result;
}

fn test_example_1() void {
    expect((myAtoi("42") == @as(i32,@intCast(42))));
}

fn test_example_2() void {
    expect((myAtoi("   -42") == (-@as(i32,@intCast(42)))));
}

fn test_example_3() void {
    expect((myAtoi("4193 with words") == @as(i32,@intCast(4193))));
}

fn test_example_4() void {
    expect((myAtoi("words and 987") == @as(i32,@intCast(0))));
}

fn test_example_5() void {
    expect((myAtoi("-91283472332") == (-@as(i32,@intCast(2147483648)))));
}

pub fn main() void {
    test_example_1();
    test_example_2();
    test_example_3();
    test_example_4();
    test_example_5();
}
