// Generated by Mochi compiler v0.10.27 on 2025-07-17T07:46:19Z
open System

exception Break
exception Continue

let numbers: int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
try
    for n in numbers do
        try
            if n % 2 = 0 then
                raise Continue
            if n > 7 then
                raise Break
            printfn "%s" (String.concat " " [string "odd number:"; string n])
        with Continue -> ()
    with Break -> ()
