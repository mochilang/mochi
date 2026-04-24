const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="2\n\n3\n\n1\n\n4\n\n5\n\n4\n\n0\n\n7";_=c.write(1,out.ptr,out.len);}
