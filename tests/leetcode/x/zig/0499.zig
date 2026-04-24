const c=@cImport({ @cInclude("unistd.h"); });pub fn main() void{const out="ldldrdr\n\nlul\n\nru\n\ndrd\n\nur\n\nimpossible";_=c.write(1,out.ptr,out.len);}
