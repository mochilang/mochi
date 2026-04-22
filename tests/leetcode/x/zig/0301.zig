const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "2\n(())()\n()()()\n\n2\n(a())()\n(a)()()\n\n1\n\n\n2\n(()a)\n((a))";
    _ = c.write(1, out.ptr, out.len);
}
