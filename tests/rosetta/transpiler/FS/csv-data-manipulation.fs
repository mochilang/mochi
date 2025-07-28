// Generated 2025-07-28 11:14 +0700

exception Return

let rec join (xs: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    let mutable sep = sep
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            if i > 0 then
                res <- res + sep
            res <- res + (xs.[i])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length str) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- int ((n * 10) + (int (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>)))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
let mutable rows: string array array = [|[|"A"; "B"; "C"|]; [|"1"; "2"; "3"|]; [|"4"; "5"; "6"|]; [|"7"; "8"; "9"|]|]
rows.[0] <- Array.append (rows.[0]) [|"SUM"|]
let mutable i: int = 1
while i < (Seq.length rows) do
    let mutable sum: int = 0
    for s in rows.[i] do
        sum <- sum + (int (parseIntStr s))
    rows.[i] <- Array.append (rows.[i]) [|string sum|]
    i <- i + 1
for r in rows do
    printfn "%s" (join r ",")
