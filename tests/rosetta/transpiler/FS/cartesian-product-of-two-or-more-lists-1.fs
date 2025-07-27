// Generated 2025-07-27 22:40 +0700

exception Return

let rec cart2 (a: int array) (b: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        let mutable p: int array array = [||]
        for x in a do
            for y in b do
                p <- unbox<int array array> (Array.append p [|[|x; y|]|])
        __ret <- unbox<int array array> p
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
            let mutable row: int array = lst.[i]
            s <- s + "["
            let mutable j: int = 0
            while j < (unbox<int> (Array.length row)) do
                s <- s + (string (row.[j]))
                if j < (unbox<int> ((unbox<int> (Array.length row)) - 1)) then
                    s <- s + " "
                j <- j + 1
            s <- s + "]"
            if i < (unbox<int> ((unbox<int> (Array.length lst)) - 1)) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" (llStr (cart2 [|1; 2|] [|3; 4|]))
        printfn "%s" (llStr (cart2 [|3; 4|] [|1; 2|]))
        printfn "%s" (llStr (cart2 [|1; 2|] [||]))
        printfn "%s" (llStr (cart2 [||] [|1; 2|]))
        __ret
    with
        | Return -> __ret
main()
