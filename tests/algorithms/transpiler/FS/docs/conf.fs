// Generated 2025-08-07 15:46 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec parse_project_name (toml: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable toml = toml
    try
        let mutable i: int = 0
        let mutable name: string = ""
        let n: int = String.length (toml)
        while (i + 4) < n do
            if ((((string (toml.[i])) = "n") && ((string (toml.[i + 1])) = "a")) && ((string (toml.[i + 2])) = "m")) && ((string (toml.[i + 3])) = "e") then
                i <- i + 4
                while (i < n) && ((string (toml.[i])) <> "\"") do
                    i <- i + 1
                i <- i + 1
                while (i < n) && ((string (toml.[i])) <> "\"") do
                    name <- name + (string (toml.[i]))
                    i <- i + 1
                __ret <- name
                raise Return
            i <- i + 1
        __ret <- name
        raise Return
        __ret
    with
        | Return -> __ret
let pyproject: string = "[project]\nname = \"thealgorithms-python\""
let project: string = parse_project_name (pyproject)
printfn "%s" (project)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
