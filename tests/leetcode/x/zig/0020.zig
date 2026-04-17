const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn isValid(s: []const u8) bool {
    var stack: [10005]u8 = undefined;
    var top: usize = 0;
    for (s) |ch| {
        if (ch == '(' or ch == '[' or ch == '{') {
            stack[top] = ch;
            top += 1;
        } else {
            if (top == 0) return false;
            top -= 1;
            const open = stack[top];
            if ((ch == ')' and open != '(') or
                (ch == ']' and open != '[') or
                (ch == '}' and open != '{')) return false;
        }
    }
    return top == 0;
}

pub fn main() !void {
    var buf: [1 << 20]u8 = undefined;
    const read_n = c.read(0, &buf, buf.len);
    if (read_n <= 0) return;
    const input = buf[0..@intCast(read_n)];
    var tokens = std.mem.tokenizeAny(u8, input, " \n\r\t");
    const t = try std.fmt.parseInt(usize, tokens.next().?, 10);
    var i: usize = 0;
    while (i < t) : (i += 1) {
        const s = tokens.next().?;
        const line = if (isValid(s))
            if (i + 1 < t) "true\n" else "true"
        else
            if (i + 1 < t) "false\n" else "false";
        _ = c.write(1, line.ptr, line.len);
    }
}
