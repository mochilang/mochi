// Generated 2025-07-25 01:36 +0700

exception Return

let rec removeKey (m: Map<string, int>) (k: string) =
    let mutable __ret : Map<string, int> = Unchecked.defaultof<Map<string, int>>
    let mutable m = m
    let mutable k = k
    try
        let mutable out: Map<string, int> = Map.ofList []
        for KeyValue(key, _) in m do
            if key <> k then
                out <- Map.add key ((defaultArg (Map.tryFind key m) Unchecked.defaultof<int>)) out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable x: Map<string, int> = Unchecked.defaultof<Map<string, int>>
        x <- Map.ofList []
        x <- Map.add "foo" 3 x
        let y1 = (defaultArg (Map.tryFind "bar" x) Unchecked.defaultof<int>)
        let ok: bool = Map.containsKey "bar" x
        printfn "%A" y1
        printfn "%d" (if ok then 1 else 0)
        x <- removeKey x "foo"
        x <- Map.ofList [("foo", 2); ("bar", 42); ("baz", -1)]
        printfn "%s" (String.concat " " [|sprintf "%A" ((defaultArg (Map.tryFind "foo" x) Unchecked.defaultof<int>)); sprintf "%A" ((defaultArg (Map.tryFind "bar" x) Unchecked.defaultof<int>)); sprintf "%A" ((defaultArg (Map.tryFind "baz" x) Unchecked.defaultof<int>))|])
        __ret
    with
        | Return -> __ret
main()
