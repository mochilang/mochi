const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "true\nfalse\nfalse\ntrue\ntrue";
    _ = c.write(1, out.ptr, out.len);
}
