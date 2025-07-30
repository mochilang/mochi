// Generated 2025-07-30 21:41 +0700

exception Break
exception Continue

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec modPow (``base``: int) (exp: int) (m: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    let mutable m = m
    try
        let mutable result: int = ((1 % m + m) % m)
        let mutable b: int = ((``base`` % m + m) % m)
        let mutable e: int = exp
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % m + m) % m)
            b <- (((b * b) % m + m) % m)
            e <- int (e / 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        for p in [|2; 3; 5; 7; 11; 13; 17; 19; 23; 29|] do
            if (int (((n % p + p) % p))) = 0 then
                __ret <- n = (int p)
                raise Return
        let mutable d: int = n - 1
        let mutable s: int = 0
        while (((d % 2 + 2) % 2)) = 0 do
            d <- d / 2
            s <- s + 1
        try
            for a in [|2; 325; 9375; 28178; 450775; 9780504; 1795265022|] do
                try
                    if (int (((a % n + n) % n))) = 0 then
                        __ret <- true
                        raise Return
                    let mutable x: int = modPow (int a) d n
                    if (x = 1) || (x = (n - 1)) then
                        raise Continue
                    let mutable r: int = 1
                    let mutable passed: bool = false
                    try
                        while r < s do
                            try
                                x <- (((x * x) % n + n) % n)
                                if x = (n - 1) then
                                    passed <- true
                                    raise Break
                                r <- r + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not passed then
                        __ret <- false
                        raise Return
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and commatize (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let mutable s: string = string n
        let mutable i: int = (String.length s) - 3
        while i > 0 do
            s <- ((_substring s 0 i) + ",") + (_substring s i (String.length s))
            i <- i - 3
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and pad (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable out: string = s
        while (String.length out) < width do
            out <- " " + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and join (xs: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    let mutable sep = sep
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            if i > 0 then
                res <- res + sep
            res <- res + (xs.[i])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and formatRow (row: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    try
        let mutable padded: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length row) do
            padded <- Array.append padded [|unbox<string> (pad (row.[i]) 9)|]
            i <- i + 1
        __ret <- ("[" + (unbox<string> (join padded " "))) + "]"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable cubans: string array = [||]
        let mutable cube1: int = 1
        let mutable count: int = 0
        let mutable cube100k: int = 0
        let mutable i: int = 1
        try
            while true do
                try
                    let mutable j: int = i + 1
                    let mutable cube2: int = (j * j) * j
                    let mutable diff: int = cube2 - cube1
                    if isPrime diff then
                        if count < 200 then
                            cubans <- Array.append cubans [|unbox<string> (commatize diff)|]
                        count <- count + 1
                        if count = 100000 then
                            cube100k <- diff
                            raise Break
                    cube1 <- cube2
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        printfn "%s" "The first 200 cuban primes are:-"
        let mutable row: int = 0
        while row < 20 do
            let mutable slice: string array = [||]
            let mutable k: int = 0
            while k < 10 do
                slice <- Array.append slice [|cubans.[(row * 10) + k]|]
                k <- k + 1
            printfn "%s" (formatRow slice)
            row <- row + 1
        printfn "%s" ("\nThe 100,000th cuban prime is " + (unbox<string> (commatize cube100k)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
