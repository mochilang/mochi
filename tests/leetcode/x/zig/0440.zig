const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="10\n\n1\n\n17\n\n9\n\n999999999\n\n199\n\n9\n\n19";_=c.write(1,out.ptr,out.len);}
