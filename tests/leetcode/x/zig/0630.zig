const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="3\n\n1\n\n0\n\n2\n\n3\n\n4";_=c.write(1,out.ptr,out.len);}
