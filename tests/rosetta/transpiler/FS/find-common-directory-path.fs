// Generated 2025-08-05 01:14 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec splitPath (p: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable p = p
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length(p)) do
            if (_substring p i (i + 1)) = "/" then
                if cur <> "" then
                    parts <- Array.append parts [|cur|]
                    cur <- ""
            else
                cur <- cur + (_substring p i (i + 1))
            i <- i + 1
        if cur <> "" then
            parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and joinPath (parts: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable parts = parts
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length(parts)) do
            s <- (s + "/") + (_idx parts (i))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and commonPrefix (paths: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable paths = paths
    try
        if (Seq.length(paths)) = 0 then
            __ret <- ""
            raise Return
        let mutable ``base``: string array = splitPath (_idx paths (0))
        let mutable i: int = 0
        let mutable prefix: string array = [||]
        try
            while i < (Seq.length(``base``)) do
                try
                    let comp: string = _idx ``base`` (i)
                    let mutable ok: bool = true
                    try
                        for p in Seq.map string (paths) do
                            try
                                let mutable parts: string array = splitPath (p)
                                if (i >= (Seq.length(parts))) || ((_idx parts (i)) <> comp) then
                                    ok <- false
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if ok then
                        prefix <- Array.append prefix [|comp|]
                    else
                        raise Break
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- joinPath (prefix)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let paths: string array = [|"/home/user1/tmp/coverage/test"; "/home/user1/tmp/covert/operator"; "/home/user1/tmp/coven/members"; "/home//user1/tmp/coventry"; "/home/user1/././tmp/covertly/foo"; "/home/bob/../user1/tmp/coved/bar"|]
        let c: string = commonPrefix (paths)
        if c = "" then
            printfn "%s" ("No common path")
        else
            printfn "%s" ("Common path: " + c)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
