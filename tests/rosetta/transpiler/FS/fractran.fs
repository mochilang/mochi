// Generated 2025-08-01 12:46 +0000

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

type StepResult = {
    n: bigint
    ok: bool
}
let rec step (n: bigint) (program: bigint array array) =
    let mutable __ret : StepResult = Unchecked.defaultof<StepResult>
    let mutable n = n
    let mutable program = program
    try
        let mutable i: int = 0
        while i < (Seq.length program) do
            let num: bigint = (program.[i]).[0]
            let den: bigint = (program.[i]).[1]
            if (((n % den + den) % den)) = (bigint 0) then
                n <- (n / den) * num
                __ret <- { n = n; ok = true }
                raise Return
            i <- i + 1
        __ret <- { n = n; ok = false }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let program: bigint array array = [|[|bigint 17; bigint 91|]; [|bigint 78; bigint 85|]; [|bigint 19; bigint 51|]; [|bigint 23; bigint 38|]; [|bigint 29; bigint 33|]; [|bigint 77; bigint 29|]; [|bigint 95; bigint 23|]; [|bigint 77; bigint 19|]; [|bigint 1; bigint 17|]; [|bigint 11; bigint 13|]; [|bigint 13; bigint 11|]; [|bigint 15; bigint 14|]; [|bigint 15; bigint 2|]; [|bigint 55; bigint 1|]|]
        let mutable n: bigint = bigint 2
        let mutable primes: int = 0
        let mutable count: int = 0
        let limit: int = 1000000
        let two: bigint = bigint 2
        let mutable line: string = ""
        try
            while (primes < 20) && (count < limit) do
                try
                    let res: StepResult = step n program
                    n <- res.n
                    if not (res.ok) then
                        raise Break
                    let mutable m: bigint = n
                    let mutable pow: int = 0
                    while (((m % two + two) % two)) = (bigint 0) do
                        m <- m / two
                        pow <- pow + 1
                    if (m = (bigint 1)) && (pow > 1) then
                        line <- (line + (string pow)) + " "
                        primes <- primes + 1
                    count <- count + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if (String.length line) > 0 then
            printfn "%s" (_substring line 0 ((String.length line) - 1))
        else
            printfn "%s" ""
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
