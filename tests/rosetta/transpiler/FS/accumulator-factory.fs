// Generated 2025-07-24 20:52 +0700

exception Return

let rec accumulator (sum: obj) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable sum = sum
    try
        let mutable store: obj array = [|sum|]
        let rec add (nv: obj) =
            let mutable __ret = ()
            let mutable nv = nv
            try
                store.[0] <- (store.[0]) + nv
                __ret <- store.[0]
                raise Return
                __ret
            with
                | Return -> __ret
        __ret <- add
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let x = accumulator 1
        x 5
        accumulator 3
        printfn "%s" (string (x 2.3))
        __ret
    with
        | Return -> __ret
main()
