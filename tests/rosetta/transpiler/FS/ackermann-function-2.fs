// Generated 2025-07-24 20:52 +0700

exception Return

let rec pow (base: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable base = base
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * base
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and ackermann2 (m: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    let mutable n = n
    try
        if m = 0 then
            __ret <- n + 1
            raise Return
        if m = 1 then
            __ret <- n + 2
            raise Return
        if m = 2 then
            __ret <- (2 * n) + 3
            raise Return
        if m = 3 then
            __ret <- (8 * (pow 2 n)) - 3
            raise Return
        if n = 0 then
            __ret <- ackermann2 (m - 1) 1
            raise Return
        __ret <- ackermann2 (m - 1) (ackermann2 m (n - 1))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        printfn "%s" ("A(0, 0) = " + (string (ackermann2 0 0)))
        printfn "%s" ("A(1, 2) = " + (string (ackermann2 1 2)))
        printfn "%s" ("A(2, 4) = " + (string (ackermann2 2 4)))
        printfn "%s" ("A(3, 4) = " + (string (ackermann2 3 4)))
        __ret
    with
        | Return -> __ret
main()
