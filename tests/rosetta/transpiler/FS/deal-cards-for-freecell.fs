// Generated 2025-07-31 00:10 +0700

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

let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable seed: int = 1
let rec rnd () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        seed <- ((((seed * 214013) + 2531011) % (int 2147483648L) + (int 2147483648L)) % (int 2147483648L))
        __ret <- seed / 65536
        raise Return
        __ret
    with
        | Return -> __ret
let rec deal (game: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable game = game
    try
        seed <- game
        let mutable deck: int array = [||]
        let mutable i: int = 0
        while i < 52 do
            deck <- Array.append deck [|51 - i|]
            i <- i + 1
        i <- 0
        while i < 51 do
            let j: int = 51 - (int ((((rnd()) % (52 - i) + (52 - i)) % (52 - i))))
            let tmp: int = deck.[i]
            deck.[i] <- deck.[j]
            deck.[j] <- tmp
            i <- i + 1
        __ret <- deck
        raise Return
        __ret
    with
        | Return -> __ret
let suits: string = "CDHS"
let nums: string = "A23456789TJQK"
let rec show (cards: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable cards = cards
    try
        let mutable i: int = 0
        while i < (Seq.length cards) do
            let c: int = cards.[i]
            printf "%s" ((" " + (_substring nums (c / 4) ((c / 4) + 1))) + (_substring suits (((c % 4 + 4) % 4)) ((((c % 4 + 4) % 4)) + 1)))
            if (((((i + 1) % 8 + 8) % 8)) = 0) || ((i + 1) = (Seq.length cards)) then
                printfn "%s" ""
            i <- i + 1
        __ret
    with
        | Return -> __ret
printfn "%s" ""
printfn "%s" "Game #1"
show (deal 1)
printfn "%s" ""
printfn "%s" "Game #617"
show (deal 617)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
