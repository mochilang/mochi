// Generated 2025-07-25 13:04 +0700

exception Return

let rec ackermann (m: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    let mutable n = n
    try
        if m = 0 then
            __ret <- n + 1
            raise Return
        if n = 0 then
            __ret <- ackermann (m - 1) 1
            raise Return
        __ret <- ackermann (m - 1) (ackermann m (n - 1))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" ("A(0, 0) = " + (string (ackermann 0 0)))
        printfn "%s" ("A(1, 2) = " + (string (ackermann 1 2)))
        printfn "%s" ("A(2, 4) = " + (string (ackermann 2 4)))
        printfn "%s" ("A(3, 4) = " + (string (ackermann 3 4)))
        __ret
    with
        | Return -> __ret
main()
