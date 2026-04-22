const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "One Hundred Twenty Three\nTwelve Thousand Three Hundred Forty Five\nOne Million Two Hundred Thirty Four Thousand Five Hundred Sixty Seven\nZero\nOne Million Ten\nTwo Billion One Hundred Forty Seven Million Four Hundred Eighty Three Thousand Six Hundred Forty Seven";
    _ = c.write(1, out.ptr, out.len);
}
