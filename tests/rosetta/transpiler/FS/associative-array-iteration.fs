// Generated 2025-07-24 18:50 +0000

let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let m: Map<string, int> = Map.ofList [("hello", 13); ("world", 31); ("!", 71)]
        for k in List.map fst (Map.toList m) do
            printfn "%s" ((("key = " + k) + ", value = ") + (string ((defaultArg (Map.tryFind k m) Unchecked.defaultof<int>))))
        for k in List.map fst (Map.toList m) do
            printfn "%s" ("key = " + k)
        for k in List.map fst (Map.toList m) do
            printfn "%s" ("value = " + (string ((defaultArg (Map.tryFind k m) Unchecked.defaultof<int>))))
        __ret
    with
        | Return -> __ret
main()
