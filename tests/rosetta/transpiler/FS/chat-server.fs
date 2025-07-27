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
let rec removeName (names: string array) (name: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable names = names
    let mutable name = name
    try
        let mutable out: string array = [||]
        for n in names do
            if n <> name then
                out <- unbox<string array> (Array.append out [|n|])
        __ret <- unbox<string array> out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable clients: string array = [||]
        let rec broadcast (msg: string) =
            let mutable __ret = ()
            let mutable msg = msg
            try
                printfn "%s" msg
                __ret
            with
                | Return -> __ret
        let rec add (name: string) =
            let mutable __ret = ()
            let mutable name = name
            try
                clients <- unbox<string array> (Array.append clients [|name|])
                broadcast (("+++ \"" + name) + "\" connected +++\n")
                __ret
            with
                | Return -> __ret
        let rec send (name: string) (msg: string) =
            let mutable __ret = ()
            let mutable name = name
            let mutable msg = msg
            try
                broadcast (((name + "> ") + msg) + "\n")
                __ret
            with
                | Return -> __ret
        let rec remove (name: string) =
            let mutable __ret = ()
            let mutable name = name
            try
                clients <- removeName clients name
                broadcast (("--- \"" + name) + "\" disconnected ---\n")
                __ret
            with
                | Return -> __ret
        add "Alice"
        add "Bob"
        send "Alice" "Hello Bob!"
        send "Bob" "Hi Alice!"
        remove "Bob"
        remove "Alice"
        broadcast "Server stopping!\n"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
