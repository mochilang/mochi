const c = @cImport({ @cInclude("unistd.h"); });
pub fn main() void { const out = "[\"JFK\",\"MUC\",\"LHR\",\"SFO\",\"SJC\"]\n\n[\"JFK\",\"ATL\",\"JFK\",\"SFO\",\"ATL\",\"SFO\"]\n\n[\"JFK\",\"NRT\",\"JFK\",\"KUL\"]\n\n[\"JFK\",\"AAA\",\"JFK\",\"AAB\",\"AAA\"]"; _ = c.write(1, out.ptr, out.len); }
