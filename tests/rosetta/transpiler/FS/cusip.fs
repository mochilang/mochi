// Generated 2025-07-30 21:41 +0700

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
let parseIntStr (s:string) (b:int) : int =
    System.Convert.ToInt32(s, b)

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        if (ch >= "0") && (ch <= "9") then
            __ret <- (int (parseIntStr ch 10)) + 48
            raise Return
        let mutable idx: int = upper.IndexOf(ch)
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec isCusip (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        if (String.length s) <> 9 then
            __ret <- false
            raise Return
        let mutable sum: int = 0
        let mutable i: int = 0
        while i < 8 do
            let c: string = s.Substring(i, (i + 1) - i)
            let mutable v: int = 0
            if (c >= "0") && (c <= "9") then
                v <- parseIntStr c 10
            else
                if (c >= "A") && (c <= "Z") then
                    v <- (int (ord c)) - 55
                else
                    if c = "*" then
                        v <- 36
                    else
                        if c = "@" then
                            v <- 37
                        else
                            if c = "#" then
                                v <- 38
                            else
                                __ret <- false
                                raise Return
            if (((i % 2 + 2) % 2)) = 1 then
                v <- v * 2
            sum <- (sum + (v / 10)) + (((v % 10 + 10) % 10))
            i <- i + 1
        __ret <- (int (parseIntStr (s.Substring(8, 9 - 8)) 10)) = ((((10 - (((sum % 10 + 10) % 10))) % 10 + 10) % 10))
        raise Return
        __ret
    with
        | Return -> __ret
let candidates: string array = [|"037833100"; "17275R102"; "38259P508"; "594918104"; "68389X106"; "68389X105"|]
for cand in candidates do
    let mutable b: string = "incorrect"
    if isCusip cand then
        b <- "correct"
    printfn "%s" ((cand + " -> ") + b)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
