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
type Fn() = class end
type Delegator = {
    ``delegate``: Map<string, unit -> string>
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec operation (d: Delegator) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable d = d
    try
        __ret <- if Map.containsKey "thing" (d.``delegate``) then (unbox<string> (d.``delegate``.["thing"] |> unbox<unit -> string>) ()) else "default implementation"
        raise Return
        __ret
    with
        | Return -> __ret
let rec newDelegate () =
    let mutable __ret : Map<string, unit -> string> = Unchecked.defaultof<Map<string, unit -> string>>
    try
        let mutable m: Map<string, unit -> string> = Map.ofList []
        m <- Map.add "thing" (        fun () -> 
            __ret <- unbox<Map<string, unit -> string>> "delegate implementation"
            raise Return) m
        __ret <- unbox<Map<string, unit -> string>> m
        raise Return
        __ret
    with
        | Return -> __ret
let mutable a: Delegator = { ``delegate`` = Map.ofList [] }
printfn "%s" (operation a)
a <- { a with ``delegate`` = Map.ofList [] }
printfn "%s" (operation a)
a <- { a with ``delegate`` = newDelegate() }
printfn "%s" (operation a)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
