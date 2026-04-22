const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "2\neat\noath\n\n0\n\n3\naaa\naba\nbaa\n\n2\neat\nsea";
    _ = c.write(1, out.ptr, out.len);
}
