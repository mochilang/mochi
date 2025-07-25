// Generated 2025-07-26 04:38 +0700

exception Return

let rec strdup (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        __ret <- s + ""
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let go1: string = "hello C"
        let c2: string = strdup go1
        printfn "%s" c2
        __ret
    with
        | Return -> __ret
main()
