// Generated 2025-07-28 10:03 +0700

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
type DivResult = {
    q: int
    r: int
}
let rec egyptianDivide (dividend: int) (divisor: int) =
    let mutable __ret : DivResult = Unchecked.defaultof<DivResult>
    let mutable dividend = dividend
    let mutable divisor = divisor
    try
        if (dividend < 0) || (divisor <= 0) then
            panic "Invalid argument(s)"
        if dividend < divisor then
            __ret <- { q = 0; r = dividend }
            raise Return
        let mutable powers: int array = [|1|]
        let mutable doublings: int array = [|divisor|]
        let mutable doubling: int = divisor * 2
        while doubling <= dividend do
            powers <- Array.append powers [|(powers.[(Seq.length powers) - 1]) * 2|]
            doublings <- Array.append doublings [|doubling|]
            doubling <- doubling * 2
        let mutable ans: int = 0
        let mutable accum: int = 0
        let mutable i: int = (Seq.length doublings) - 1
        try
            while i >= 0 do
                try
                    if (accum + (doublings.[i])) <= dividend then
                        accum <- accum + (doublings.[i])
                        ans <- ans + (powers.[i])
                        if accum = dividend then
                            raise Break
                    i <- i - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- { q = ans; r = dividend - accum }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let dividend: int = 580
        let divisor: int = 34
        let res: DivResult = egyptianDivide dividend divisor
        printfn "%s" (((((((string dividend) + " divided by ") + (string divisor)) + " is ") + (string (res.q))) + " with remainder ") + (string (res.r)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
