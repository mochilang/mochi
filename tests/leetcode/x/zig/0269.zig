const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "wertf\nzx\n\n\nabcd";
    _ = c.write(1, out.ptr, out.len);
}
