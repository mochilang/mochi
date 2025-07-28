// Generated 2025-07-28 11:14 +0700

let rec createFile (fs: Map<string, bool>) (fn: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable fs = fs
    let mutable fn = fn
    try
        if Map.containsKey fn fs then
            printfn "%s" (("open " + fn) + ": file exists")
        else
            fs <- Map.add fn false fs
            printfn "%s" (("file " + fn) + " created!")
        __ret
    with
        | Return -> __ret
and createDir (fs: Map<string, bool>) (dn: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable fs = fs
    let mutable dn = dn
    try
        if Map.containsKey dn fs then
            printfn "%s" (("mkdir " + dn) + ": file exists")
        else
            fs <- Map.add dn true fs
            printfn "%s" (("directory " + dn) + " created!")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable fs: Map<string, bool> = Map.ofList []
        fs <- Map.add "docs" true fs
        createFile fs "input.txt"
        createFile fs "/input.txt"
        createDir fs "docs"
        createDir fs "/docs"
        __ret
    with
        | Return -> __ret
main()
