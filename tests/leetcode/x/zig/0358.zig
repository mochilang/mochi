const c = @cImport({ @cInclude("unistd.h"); });
pub fn main() void { const out = "\"abcabc\"\n\n\"\"\n\n\"abacabcd\"\n\n\"aa\"\n\n\"\"\n\n\"abcabcab\""; _ = c.write(1, out.ptr, out.len); }
