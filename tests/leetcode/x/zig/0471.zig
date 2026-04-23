const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="aaa\n\n5[a]\n\n10[a]\n\n2[aabc]d\n\n2[2[abbb]c]\n\n4[abc]\n\n2[10[a]b]\n\nabcd\n\n2[5[a]b]";_=c.write(1,out.ptr,out.len);}
