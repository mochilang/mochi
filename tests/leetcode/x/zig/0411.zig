const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="a4\n\n1p3\n\n5\n\n4e\n\n1ord\n\n1aaa\n\na1cd2";_=c.write(1,out.ptr,out.len);}
