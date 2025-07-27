// Generated 2025-07-27 22:40 +0700

exception Return

let rec listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (unbox<int> (Array.length xs)) do
            s <- s + (string (xs.[i]))
            if i < (unbox<int> ((unbox<int> (Array.length xs)) - 1)) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and llStr (lst: int array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable lst = lst
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (unbox<int> (Array.length lst)) do
            s <- s + (unbox<string> (listStr (unbox<int array> (lst.[i]))))
            if i < (unbox<int> ((unbox<int> (Array.length lst)) - 1)) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and concat (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        let mutable out: int array = [||]
        for v in a do
            out <- unbox<int array> (Array.append out [|v|])
        for v in b do
            out <- unbox<int array> (Array.append out [|v|])
        __ret <- unbox<int array> out
        raise Return
        __ret
    with
        | Return -> __ret
and cartN (lists: obj) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable lists = lists
    try
        if lists = null then
            __ret <- unbox<int array array> [||]
            raise Return
        let a: int array array = unbox<int array array> lists
        if (unbox<int> (Array.length a)) = 0 then
            __ret <- unbox<int array array> [|[||]|]
            raise Return
        let mutable out: int array array = [||]
        let rest: int array array = cartN (Array.sub a 1 ((unbox<int> (Array.length a)) - 1))
        for x in a.[0] do
            for p in rest do
                out <- unbox<int array array> (Array.append out [|concat [|x|] (unbox<int array> p)|])
        __ret <- unbox<int array array> out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" (llStr (cartN [|[|1; 2|]; [|3; 4|]|]))
        printfn "%s" (llStr (cartN [|[|3; 4|]; [|1; 2|]|]))
        printfn "%s" (llStr (cartN [|[|1; 2|]; [||]|]))
        printfn "%s" (llStr (cartN [|[||]; [|1; 2|]|]))
        printfn "%s" ""
        printfn "%s" "["
        for p in cartN [|[|1776; 1789|]; [|7; 12|]; [|4; 14; 23|]; [|0; 1|]|] do
            printfn "%s" (" " + (unbox<string> (listStr (unbox<int array> p))))
        printfn "%s" "]"
        printfn "%s" (llStr (cartN [|[|1; 2; 3|]; [|30|]; [|500; 100|]|]))
        printfn "%s" (llStr (cartN [|[|1; 2; 3|]; [||]; [|500; 100|]|]))
        printfn "%s" ""
        printfn "%s" (llStr (cartN null))
        printfn "%s" (llStr (cartN [||]))
        __ret
    with
        | Return -> __ret
main()
