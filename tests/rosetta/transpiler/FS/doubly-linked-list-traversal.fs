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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable nodes: Map<int, Map<string, obj>> = Map.ofList []
let mutable head: int = 0 - 1
let mutable tail: int = 0 - 1
let rec listString () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        if head = (0 - 1) then
            __ret <- "<nil>"
            raise Return
        let mutable r: string = "[" + (unbox<string> ((nodes.[head] |> unbox<obj>>).["value"]))
        let mutable id: int = int ((nodes.[head] |> unbox<obj>>).["next"])
        while id <> (0 - 1) do
            r <- (r + " ") + (unbox<string> ((nodes.[id] |> unbox<obj>>).["value"]))
            id <- int ((nodes.[id] |> unbox<obj>>).["next"])
        r <- r + "]"
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (listString())
nodes <- Map.add 0 (Map.ofList [("value", box "A"); ("next", box (0 - 1)); ("prev", box (0 - 1))]) nodes
head <- 0
tail <- 0
nodes <- Map.add 1 (Map.ofList [("value", box "B"); ("next", box (0 - 1)); ("prev", box 0)]) nodes
nodes <- Map.add 0 (box (Map.add "next" (box 1) (nodes.[0] |> unbox<obj>>))) nodes
tail <- 1
printfn "%s" (listString())
nodes <- Map.add 2 (Map.ofList [("value", box "C"); ("next", box 1); ("prev", box 0)]) nodes
nodes <- Map.add 1 (box (Map.add "prev" (box 2) (nodes.[1] |> unbox<obj>>))) nodes
nodes <- Map.add 0 (box (Map.add "next" (box 2) (nodes.[0] |> unbox<obj>>))) nodes
printfn "%s" (listString())
let mutable out: string = "From tail:"
let mutable id: int = tail
while id <> (0 - 1) do
    out <- (out + " ") + (unbox<string> ((nodes.[id] |> unbox<obj>>).["value"]))
    id <- int ((nodes.[id] |> unbox<obj>>).["prev"])
printfn "%s" out
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
