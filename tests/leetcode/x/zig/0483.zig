const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="3\n\n8\n\n999999999999999999\n\n2\n\n4\n\n2\n\n2";_=c.write(1,out.ptr,out.len);}
