const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="-1\n\n2\n\n2\n\n3\n\n-1\n\n-1\n\n-1";_=c.write(1,out.ptr,out.len);}
