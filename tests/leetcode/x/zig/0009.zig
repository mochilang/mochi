const std = @import("std");
const c = @cImport({
    @cInclude("unistd.h");
});

fn isPalindrome(x: i64) bool {
    if (x < 0) return false;
    const original = x;
    var n = x;
    var rev: i64 = 0;
    while (n > 0) : (n /= 10) {
        rev = rev * 10 + @mod(n, 10);
    }
    return rev == original;
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
        const x = try std.fmt.parseInt(i64, tokens.next().?, 10);
        const line = if (isPalindrome(x))
            if (i + 1 < t) "true\n" else "true"
        else
            if (i + 1 < t) "false\n" else "false";
        _ = c.write(1, line.ptr, line.len);
    }
}
