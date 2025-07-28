// Generated 2025-07-28 11:14 +0700

exception Return

let rec listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length xs) do
            s <- s + (string (xs.[i]))
            if i < ((Seq.length xs) - 1) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and pointerDemo () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "Pointer:"
        let mutable i: int = 0
        printfn "%s" "Before:"
        printfn "%s" ((("\t<address>: " + (string i)) + ", ") + (string i))
        i <- 3
        printfn "%s" "After:"
        printfn "%s" ((("\t<address>: " + (string i)) + ", ") + (string i))
        __ret
    with
        | Return -> __ret
and sliceDemo () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "Slice:"
        let mutable a = [||]
        for _ in 0 .. (10 - 1) do
            a <- Array.append a [|0|]
        let mutable s = a
        printfn "%s" "Before:"
        printfn "%s" ("\ts: " + (unbox<string> (listStr s)))
        printfn "%s" ("\ta: " + (unbox<string> (listStr a)))
        let mutable data: int array = [|65; 32; 115; 116; 114; 105; 110; 103; 46|]
        let mutable idx: int = 0
        while idx < (Seq.length data) do
            s.[idx] <- data.[idx]
            idx <- idx + 1
        printfn "%s" "After:"
        printfn "%s" ("\ts: " + (unbox<string> (listStr s)))
        printfn "%s" ("\ta: " + (unbox<string> (listStr a)))
        __ret
    with
        | Return -> __ret
pointerDemo()
printfn "%s" ""
sliceDemo()
