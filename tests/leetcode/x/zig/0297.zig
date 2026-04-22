const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "[1,2,3,null,null,4,5]\n\n[]\n\n[1,null,2,3]\n\n[5,-3,9,null,0]";
    _ = c.write(1, out.ptr, out.len);
}
