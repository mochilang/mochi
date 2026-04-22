const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "6\n\n1\n\n9";
    _ = c.write(1, out.ptr, out.len);
}
