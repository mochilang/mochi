// Generated 2025-07-25 01:11 +0700

let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let m: Map<string, int> = Map.ofList [("hello", 13); ("world", 31); ("!", 71)]
        for k in keys m do
            printfn "%s" ((("key = " + k) + ", value = ") + (string ((defaultArg (Map.tryFind k m) Unchecked.defaultof<obj>))))
        for k in keys m do
            printfn "%s" ("key = " + k)
        for k in keys m do
            printfn "%s" ("value = " + (string ((defaultArg (Map.tryFind k m) Unchecked.defaultof<obj>))))
        __ret
    with
        | Return -> __ret
main()
