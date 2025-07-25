// Generated 2025-07-25 12:29 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System

module net =
    open System.Net

    let rec LookupHost host =
        let mutable __ret : obj array = Unchecked.defaultof<obj array>
        let mutable host = host
        try
            let addrs = Dns.GetHostAddresses host
            let mapped = Array.map (            fun ip -> (ip.ToString())) addrs
            let lst = Array.toList mapped
            __ret <- [|box lst; null|]
            raise Return
            __ret
        with
            | Return -> __ret

let res: obj array = net.LookupHost("www.kame.net")
let addrs = res.[0]
let err = res.[1]
if err = null then
    printfn "%s" (string addrs)
else
    printfn "%A" err
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
