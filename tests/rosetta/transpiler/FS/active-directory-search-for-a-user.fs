// Generated 2025-07-25 09:57 +0000

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
let rec search_user (directory: Map<string, string array>) (username: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable directory = directory
    let mutable username = username
    try
        __ret <- (defaultArg (Map.tryFind username directory) Unchecked.defaultof<string array>)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let client = Map.ofList [("Base", box "dc=example,dc=com"); ("Host", box "ldap.example.com"); ("Port", box 389); ("GroupFilter", box "(memberUid=%s)")]
        let directory: Map<string, string array> = Map.ofList [("username", [|"admins"; "users"|]); ("john", [|"users"|])]
        let groups: string array = search_user directory "username"
        if (Array.length groups) > 0 then
            let mutable out: string = "Groups: ["
            let mutable i: int = 0
            while i < (Array.length groups) do
                out <- ((out + "\"") + (groups.[i])) + "\""
                if i < ((Array.length groups) - 1) then
                    out <- out + ", "
                i <- i + 1
            out <- out + "]"
            printfn "%s" out
        else
            printfn "%s" "User not found"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
