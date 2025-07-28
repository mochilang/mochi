// Generated 2025-07-28 01:10 +0700

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
let rec quibble (items: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable items = items
    try
        let n: int = Seq.length items
        if n = 0 then
            __ret <- "{}"
            raise Return
        else
            if n = 1 then
                __ret <- ("{" + (items.[0])) + "}"
                raise Return
            else
                if n = 2 then
                    __ret <- ((("{" + (items.[0])) + " and ") + (items.[1])) + "}"
                    raise Return
                else
                    let mutable prefix: string = ""
                    try
                        for i in 0 .. ((n - 1) - 1) do
                            try
                                if i = (n - 1) then
                                    raise Break
                                if i > 0 then
                                    prefix <- prefix + ", "
                                prefix <- prefix + (items.[i])
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    __ret <- ((("{" + prefix) + " and ") + (items.[n - 1])) + "}"
                    raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (quibble [||])
        printfn "%s" (quibble [|"ABC"|])
        printfn "%s" (quibble [|"ABC"; "DEF"|])
        printfn "%s" (quibble [|"ABC"; "DEF"; "G"; "H"|])
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
