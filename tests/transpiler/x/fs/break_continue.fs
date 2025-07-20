// Generated 2025-07-20 21:29 +0700
open System

let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9]
for n in numbers do
if (n % 2) = 0 then
continue
if n > 7 then
break
printfn "%s" (String.concat " " [string "odd number:"; string n])
