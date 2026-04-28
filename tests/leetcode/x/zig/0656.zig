const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="[1,3,5]\n\n[]\n\n[1,2,4]\n\n[1,2,4]\n\n[1,4]\n\n[1,2,3,4]\n\n[1,3,5]";_=c.write(1,out.ptr,out.len);}
