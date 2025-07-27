// Generated 2025-07-27 15:42 +0000

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
type Box = {
    Contents: string
    secret: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec Box_TellSecret (self: Box) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable self = self
    try
        __ret <- self.secret
        raise Return
        __ret
    with
        | Return -> __ret
let rec newFactory () =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    try
        let mutable sn: int = 0
        let rec New () =
            let mutable __ret : Box = Unchecked.defaultof<Box>
            try
                sn <- sn + 1
                let mutable b: Box = { Contents = Unchecked.defaultof<string>; secret = sn }
                if sn = 1 then
                    b <- { b with Contents = "rabbit" }
                else
                    if sn = 2 then
                        b <- { b with Contents = "rock" }
                __ret <- b
                raise Return
                __ret
            with
                | Return -> __ret
        let rec Count () =
            let mutable __ret : int = Unchecked.defaultof<int>
            try
                __ret <- sn
                raise Return
                __ret
            with
                | Return -> __ret
        __ret <- unbox<obj array> [|box New; box Count|]
        raise Return
        __ret
    with
        | Return -> __ret
let funcs = newFactory()
let New = funcs.[0]
let Count = funcs.[1]
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
