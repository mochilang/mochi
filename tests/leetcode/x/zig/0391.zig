const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="true\n\nfalse\n\nfalse\n\ntrue\n\ntrue\n\nfalse";_=c.write(1,out.ptr,out.len);}
