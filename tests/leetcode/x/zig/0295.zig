const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "1.5\n2.0\n\n-1.0\n-1.5\n-2.0\n-2.5\n\n5.0";
    _ = c.write(1, out.ptr, out.len);
}
