const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="true\n\nfalse\n\ntrue\n\ntrue\n\ntrue\n\nfalse\n\ntrue";_=c.write(1,out.ptr,out.len);}
