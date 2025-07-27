// Generated 2025-07-27 23:36 +0700

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
let rec lower (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    try
        let up: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let low: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable i: int = 0
        while i < (String.length up) do
            if ch = (_substring up i (i + 1)) then
                __ret <- _substring low i (i + 1)
                raise Return
            i <- i + 1
        __ret <- ch
        raise Return
        __ret
    with
        | Return -> __ret
let mutable partList: string array = [|"A"; "B"; "C"; "D"|]
let mutable nAssemblies: int = 3
for p in partList do
    printfn "%s" (p + " worker running")
for cycle in 1 .. ((nAssemblies + 1) - 1) do
    printfn "%s" ("begin assembly cycle " + (string cycle))
    let mutable a: string = ""
    for p in partList do
        printfn "%s" (p + " worker begins part")
        printfn "%s" ((p + " worker completed ") + (unbox<string> (p.ToLower())))
        a <- a + (unbox<string> (p.ToLower()))
    printfn "%s" (((a + " assembled.  cycle ") + (string cycle)) + " complete")
for p in partList do
    printfn "%s" (p + " worker stopped")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
