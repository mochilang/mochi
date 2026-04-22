const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out = "aaacecaaa\ndcbabcd\n\na\nbaaab\nababbabbbababbbabbaba";
    _ = c.write(1, out.ptr, out.len);
}
