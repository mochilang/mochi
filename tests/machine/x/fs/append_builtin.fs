// Generated by Mochi compiler v0.10.27 on 2025-07-17T07:46:11Z
open System

let a: int list = [1; 2]
printfn "%s" (String.concat " " (List.map string (a @ [3])))
