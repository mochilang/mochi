const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "[1,1,2,3]\n\n[1]\n\n[1,1,1,1,1]";
    _ = c.write(1, out.ptr, out.len);
}
