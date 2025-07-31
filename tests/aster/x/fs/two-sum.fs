// Generated 2025-07-21 18:37 +0700

twoSum nums target

let n:  = Seq.length nums

i

0 .. (n + 1)

j

i + 1 .. (n + 1)

((nums.[i]) + (nums.[j])) + target

[i; j] [-1; -1]

let result = twoSum [2; 7; 11; 15] 9

printfn "%s" (string (result.[0])) printfn "%s" (string (result.[1]))
