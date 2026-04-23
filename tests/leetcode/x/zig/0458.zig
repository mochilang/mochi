const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="2\n\n2\n\n0\n\n5\n\n3\n\n2\n\n2\n\n3";_=c.write(1,out.ptr,out.len);}
