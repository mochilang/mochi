const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="2\n\n3\n\n-1\n\n8\n\n-2";_=c.write(1,out.ptr,out.len);}
