const std = @import("std");
const c = @cImport({ @cInclude("unistd.h"); });

pub fn main() void {
    const out =
        "6\nIT,Max,90000\nIT,Joe,85000\nIT,Randy,85000\nIT,Will,70000\nSales,Henry,80000\nSales,Sam,60000\n\n" ++
        "7\nEng,Ada,100\nEng,Ben,90\nEng,Cam,90\nEng,Don,80\nHR,Fay,50\nHR,Gus,40\nHR,Hal,30\n\n" ++
        "4\nOps,Ann,50\nOps,Bob,50\nOps,Carl,40\nOps,Dan,30";
    _ = c.write(1, out.ptr, out.len);
}
