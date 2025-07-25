// Generated 2025-07-25 13:04 +0700

exception Return

let rec pow_big (``base``: bigint) (exp: int) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: bigint = 1 :?> bigint
        let mutable b: bigint = ``base``
        let mutable e: int = exp
        while e > 0 do
            if (e % 2) = 1 then
                result <- result * b
            b <- b * b
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and bit_len (x: bigint) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable n: bigint = x
        let mutable c: int = 0
        while n > 0 do
            n <- n / 2
            c <- c + 1
        __ret <- c
        raise Return
        __ret
    with
        | Return -> __ret
let mutable err: string = ""
let rec ackermann2 (m: bigint) (n: bigint) =
    let mutable __ret : bigint = Unchecked.defaultof<bigint>
    let mutable m = m
    let mutable n = n
    try
        if err <> "" then
            __ret <- 0 :?> bigint
            raise Return
        if m <= 3 then
            let mi = int m
            if mi = 0 then
                __ret <- n + 1
                raise Return
            if mi = 1 then
                __ret <- n + 2
                raise Return
            if mi = 2 then
                __ret <- (2 * n) + 3
                raise Return
            if mi = 3 then
                let nb = bit_len n
                if nb > 64 then
                    err <- ("A(m,n) had n of " + (string nb)) + " bits; too large"
                    __ret <- 0 :?> bigint
                    raise Return
                let r = pow_big (2 :?> bigint) (int n)
                __ret <- (8 * r) - 3
                raise Return
        if (bit_len n) = 0 then
            __ret <- ackermann2 (m - (1 :?> bigint)) (1 :?> bigint)
            raise Return
        __ret <- ackermann2 (m - (1 :?> bigint)) (ackermann2 m (n - (1 :?> bigint)))
        raise Return
        __ret
    with
        | Return -> __ret
and show (m: int) (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable m = m
    let mutable n = n
    try
        err <- ""
        let res = ackermann2 (m :?> bigint) (n :?> bigint)
        if err <> "" then
            printfn "%s" ((((("A(" + (string m)) + ", ") + (string n)) + ") = Error: ") + err)
            __ret <- ()
            raise Return
        if (bit_len res) <= 256 then
            printfn "%s" ((((("A(" + (string m)) + ", ") + (string n)) + ") = ") + (string res))
        else
            let s: string = string res
            let pre: string = s.Substring(0, 20 - 0)
            let suf: string = s.Substring((String.length s) - 20, (String.length s) - ((String.length s) - 20))
            printfn "%s" ((((((((("A(" + (string m)) + ", ") + (string n)) + ") = ") + (string (String.length s))) + " digits starting/ending with: ") + pre) + "...") + suf)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        show 0 0
        show 1 2
        show 2 4
        show 3 100
        show 3 1000000
        show 4 1
        show 4 2
        show 4 3
        __ret
    with
        | Return -> __ret
main()
