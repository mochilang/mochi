// Generated 2025-07-28 11:14 +0700

let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let row: int = 3
        let col: int = 4
        let mutable a: int array array = [||]
        let mutable i: int = 0
        while i < row do
            let mutable rowArr: int array = [||]
            let mutable j: int = 0
            while j < col do
                rowArr <- Array.append rowArr [|0|]
                j <- j + 1
            a <- Array.append a [|rowArr|]
            i <- i + 1
        printfn "%s" ("a[0][0] = " + (string ((a.[0]).[0])))
        (a.[int (row - 1)]).[int (col - 1)] <- 7
        printfn "%s" ((((("a[" + (string (row - 1))) + "][") + (string (col - 1))) + "] = ") + (string ((a.[int (row - 1)]).[int (col - 1)])))
        a <- unbox<int array array> null
        __ret
    with
        | Return -> __ret
main()
