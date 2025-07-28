// Generated 2025-07-28 07:48 +0700

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
let rec timeStr (sec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sec = sec
    try
        let mutable wks: int = sec / 604800
        sec <- ((sec % 604800 + 604800) % 604800)
        let mutable ds: int = sec / 86400
        sec <- ((sec % 86400 + 86400) % 86400)
        let mutable hrs: int = sec / 3600
        sec <- ((sec % 3600 + 3600) % 3600)
        let mutable mins: int = sec / 60
        sec <- ((sec % 60 + 60) % 60)
        let mutable res: string = ""
        let mutable comma: bool = false
        if wks <> 0 then
            res <- (res + (string wks)) + " wk"
            comma <- true
        if ds <> 0 then
            if comma then
                res <- res + ", "
            res <- (res + (string ds)) + " d"
            comma <- true
        if hrs <> 0 then
            if comma then
                res <- res + ", "
            res <- (res + (string hrs)) + " hr"
            comma <- true
        if mins <> 0 then
            if comma then
                res <- res + ", "
            res <- (res + (string mins)) + " min"
            comma <- true
        if sec <> 0 then
            if comma then
                res <- res + ", "
            res <- (res + (string sec)) + " sec"
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (timeStr 7259)
printfn "%s" (timeStr 86400)
printfn "%s" (timeStr 6000000)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
