// Generated 2025-07-24 18:50 +0000

exception Return

let rec merge (``base``: Map<string, obj>) (update: Map<string, obj>) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable ``base`` = ``base``
    let mutable update = update
    try
        let mutable result: Map<string, obj> = Map.ofList []
        for KeyValue(k, _) in ``base`` do
            result <- Map.add k ((defaultArg (Map.tryFind k ``base``) Unchecked.defaultof<obj>)) result
        for KeyValue(k, _) in update do
            result <- Map.add k ((defaultArg (Map.tryFind k update) Unchecked.defaultof<obj>)) result
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let ``base``: Map<string, obj> = Map.ofList [("name", box "Rocket Skates"); ("price", box 12.75); ("color", box "yellow")]
        let update: Map<string, obj> = Map.ofList [("price", box 15.25); ("color", box "red"); ("year", box 1974)]
        let result = merge ``base`` update
        printfn "%A" result
        __ret
    with
        | Return -> __ret
main()
