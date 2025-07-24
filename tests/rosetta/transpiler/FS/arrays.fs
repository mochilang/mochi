// Generated 2025-07-25 01:11 +0700

exception Return

let rec listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Array.length xs) do
            s <- s + (string (xs.[i]))
            if (i + 1) < (Array.length xs) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable a: int array = [|0; 0; 0; 0; 0|]
printfn "%s" ("len(a) = " + (string (Array.length a)))
printfn "%s" ("a = " + (listStr a))
a.[0] <- 3
printfn "%s" ("a = " + (listStr a))
printfn "%s" ("a[0] = " + (string (a.[0])))
let mutable s = Array.sub a 0 (4 - 0)
let mutable cap_s: int = 5
printfn "%s" ("s = " + (listStr s))
printfn "%s" ((("len(s) = " + (string (Array.length s))) + "  cap(s) = ") + (string cap_s))
s <- Array.sub a 0 (5 - 0)
printfn "%s" ("s = " + (listStr s))
a.[0] <- 22
s.[0] <- 22
printfn "%s" ("a = " + (listStr a))
printfn "%s" ("s = " + (listStr s))
s <- Array.append s [|4|]
s <- Array.append s [|5|]
s <- Array.append s [|6|]
cap_s <- 10
printfn "%s" ("s = " + (listStr s))
printfn "%s" ((("len(s) = " + (string (Array.length s))) + "  cap(s) = ") + (string cap_s))
a.[4] <- -1
printfn "%s" ("a = " + (listStr a))
printfn "%s" ("s = " + (listStr s))
s <- [||]
for i in 0 .. (8 - 1) do
    s <- Array.append s [|0|]
cap_s <- 8
printfn "%s" ("s = " + (listStr s))
printfn "%s" ((("len(s) = " + (string (Array.length s))) + "  cap(s) = ") + (string cap_s))
