// Generated 2025-08-04 20:44 +0700

exception Return
let mutable __ret = ()

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
type Frac = {
    num: int
    den: int
}
let rec fracStr (f: Frac) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    try
        __ret <- ((string (f.num)) + "/") + (string (f.den))
        raise Return
        __ret
    with
        | Return -> __ret
and gen (l: Frac) (r: Frac) (n: int) (acc: Frac array) =
    let mutable __ret : Frac array = Unchecked.defaultof<Frac array>
    let mutable l = l
    let mutable r = r
    let mutable n = n
    let mutable acc = acc
    try
        let m: Frac = { num = (l.num) + (r.num); den = (l.den) + (r.den) }
        if (m.den) <= n then
            acc <- gen (l) (m) (n) (acc)
            acc <- Array.append acc [|m|]
            acc <- gen (m) (r) (n) (acc)
        __ret <- acc
        raise Return
        __ret
    with
        | Return -> __ret
and totient (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable tot: int = n
        let mutable nn: int = n
        let mutable p: int = 2
        while (p * p) <= nn do
            if (((nn % p + p) % p)) = 0 then
                while (((nn % p + p) % p)) = 0 do
                    nn <- nn / p
                tot <- tot - (tot / p)
            if p = 2 then
                p <- 1
            p <- p + 2
        if nn > 1 then
            tot <- tot - (tot / nn)
        __ret <- tot
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable n: int = 1
        while n <= 11 do
            let l: Frac = { num = 0; den = 1 }
            let r: Frac = { num = 1; den = 1 }
            let mutable seq: Frac array = gen (l) (r) (n) (Array.empty<Frac>)
            let mutable line: string = (("F(" + (string (n))) + "): ") + (fracStr (l))
            for f in seq do
                line <- (line + " ") + (fracStr (f))
            line <- (line + " ") + (fracStr (r))
            printfn "%s" (line)
            n <- n + 1
        let mutable sum: int = 1
        let mutable i: int = 1
        let mutable next: int = 100
        while i <= 1000 do
            sum <- sum + (totient (i))
            if i = next then
                printfn "%s" ((("|F(" + (string (i))) + ")|: ") + (string (sum)))
                next <- next + 100
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
