// Generated 2025-07-25 13:04 +0700

exception Return

let rec accumulator (sum: obj) =
    let mutable __ret : obj -> unit = Unchecked.defaultof<obj -> unit>
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
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let x = accumulator 1
        x 5
        accumulator 3
        printfn "%s" (string (x 2.3))
        __ret
    with
        | Return -> __ret
main()
