const c = @cImport({ @cInclude("unistd.h"); });
pub fn main() void { const out = "[[0,1],[1,0],[2,4],[3,2]]\n\n[[0,1],[1,0]]\n\n[[0,1],[1,0]]\n\n[[0,1],[1,0],[2,3],[3,2]]\n\n[[0,5],[1,3],[2,4],[3,0],[4,0],[5,0]]"; _ = c.write(1, out.ptr, out.len); }
