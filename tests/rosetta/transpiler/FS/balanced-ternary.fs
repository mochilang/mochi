// Generated 2025-07-26 04:38 +0700

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
let rec trimLeftZeros (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable i: int = 0
        while (i < (String.length s)) && ((s.Substring(i, (i + 1) - i)) = "0") do
            i <- i + 1
        __ret <- s.Substring(i, (String.length s) - i)
        raise Return
        __ret
    with
        | Return -> __ret
and btString (s: string) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable s = s
    try
        s <- trimLeftZeros s
        let mutable b: int array = [||]
        let mutable i: int = (String.length s) - 1
        while i >= 0 do
            let ch: string = s.Substring(i, (i + 1) - i)
            if ch = "+" then
                b <- Array.append b [|1|]
            else
                if ch = "0" then
                    b <- Array.append b [|0|]
                else
                    if ch = "-" then
                        b <- Array.append b [|0 - 1|]
                    else
                        __ret <- Map.ofList [("bt", box [||]); ("ok", box false)]
                        raise Return
            i <- i - 1
        __ret <- Map.ofList [("bt", box b); ("ok", box true)]
        raise Return
        __ret
    with
        | Return -> __ret
and btToString (b: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        if (int (Array.length b)) = 0 then
            __ret <- "0"
            raise Return
        let mutable r: string = ""
        let mutable i: int = (int (Array.length b)) - 1
        while i >= 0 do
            let d: int = b.[i]
            if d = (0 - 1) then
                r <- r + "-"
            else
                if d = 0 then
                    r <- r + "0"
                else
                    r <- r + "+"
            i <- i - 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and btInt (i: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable i = i
    try
        if i = 0 then
            __ret <- [||]
            raise Return
        let mutable n: int = i
        let mutable b: int array = [||]
        while n <> 0 do
            let mutable m: int = ((n % 3 + 3) % 3)
            n <- int (n / 3)
            if m = 2 then
                m <- 0 - 1
                n <- n + 1
            else
                if m = (0 - 2) then
                    m <- 1
                    n <- n - 1
            b <- Array.append b [|m|]
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and btToInt (b: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable b = b
    try
        let mutable r: int = 0
        let mutable pt: int = 1
        let mutable i: int = 0
        while i < (int (Array.length b)) do
            r <- r + (int ((int (b.[i])) * pt))
            pt <- pt * 3
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and btNeg (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable b = b
    try
        let mutable r: int array = [||]
        let mutable i: int = 0
        while i < (int (Array.length b)) do
            r <- Array.append r [|-(b.[i])|]
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and btAdd (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        __ret <- btInt (int ((btToInt a) + (btToInt b)))
        raise Return
        __ret
    with
        | Return -> __ret
and btMul (a: int array) (b: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable b = b
    try
        __ret <- btInt (int ((btToInt a) * (btToInt b)))
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable r: string = s
        while (String.length r) < w do
            r <- " " + r
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and show (label: string) (b: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable label = label
    let mutable b = b
    try
        let l: string = padLeft label 7
        let bs: string = padLeft (unbox<string> (btToString b)) 12
        let is: string = padLeft (string (btToInt b)) 7
        printfn "%s" ((((l + " ") + bs) + " ") + is)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let ares: Map<string, obj> = btString "+-0++0+"
        let a: obj = ares.["bt"]
        let b: int array = btInt (-436)
        let cres: Map<string, obj> = btString "+-++-"
        let c: obj = cres.["bt"]
        show "a:" (unbox<int array> a)
        show "b:" b
        show "c:" (unbox<int array> c)
        show "a(b-c):" (unbox<int array> (btMul (unbox<int array> a) (unbox<int array> (btAdd b (unbox<int array> (btNeg (unbox<int array> c)))))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
