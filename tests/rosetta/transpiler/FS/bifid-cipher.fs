// Generated 2025-07-26 04:38 +0700

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
let rec square_to_maps (square: string array array) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable square = square
    try
        let mutable emap: Map<string, int array> = Map.ofList []
        let mutable dmap: Map<string, string> = Map.ofList []
        let mutable x: int = 0
        while x < (int (Array.length square)) do
            let row: string array = square.[x]
            let mutable y: int = 0
            while y < (int (Array.length row)) do
                let ch: string = row.[y]
                emap <- Map.add ch [|x; y|] emap
                dmap <- Map.add (((string x) + ",") + (string y)) ch dmap
                y <- y + 1
            x <- x + 1
        __ret <- Map.ofList [("e", box emap); ("d", box dmap)]
        raise Return
        __ret
    with
        | Return -> __ret
and remove_space (text: string) (emap: Map<string, int array>) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable emap = emap
    try
        let s: string = text.ToUpper()
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = s.Substring(i, (i + 1) - i)
            if (ch <> " ") && (Map.containsKey ch emap) then
                out <- out + ch
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and encrypt (text: string) (emap: Map<string, int array>) (dmap: Map<string, string>) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable emap = emap
    let mutable dmap = dmap
    try
        text <- remove_space text emap
        let mutable row0: int array = [||]
        let mutable row1: int array = [||]
        let mutable i: int = 0
        while i < (String.length text) do
            let ch: string = text.Substring(i, (i + 1) - i)
            let xy: int array = emap.[ch] |> unbox<int array>
            row0 <- Array.append row0 [|xy.[0]|]
            row1 <- Array.append row1 [|xy.[1]|]
            i <- i + 1
        for v in row1 do
            row0 <- Array.append row0 [|v|]
        let mutable res: string = ""
        let mutable j: int = 0
        while j < (int (Array.length row0)) do
            let key: string = ((string (row0.[j])) + ",") + (string (row0.[j + 1]))
            res <- res + (unbox<string> (dmap.[key] |> unbox<string>))
            j <- j + 2
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt (text: string) (emap: Map<string, int array>) (dmap: Map<string, string>) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable emap = emap
    let mutable dmap = dmap
    try
        text <- remove_space text emap
        let mutable coords: int array = [||]
        let mutable i: int = 0
        while i < (String.length text) do
            let ch: string = text.Substring(i, (i + 1) - i)
            let xy: int array = emap.[ch] |> unbox<int array>
            coords <- Array.append coords [|xy.[0]|]
            coords <- Array.append coords [|xy.[1]|]
            i <- i + 1
        let mutable half: int = (int (Array.length coords)) / 2
        let mutable k1: int array = [||]
        let mutable k2: int array = [||]
        let mutable idx: int = 0
        while idx < half do
            k1 <- Array.append k1 [|coords.[idx]|]
            idx <- idx + 1
        while idx < (int (Array.length coords)) do
            k2 <- Array.append k2 [|coords.[idx]|]
            idx <- idx + 1
        let mutable res: string = ""
        let mutable j: int = 0
        while j < half do
            let key: string = ((string (k1.[j])) + ",") + (string (k2.[j]))
            res <- res + (unbox<string> (dmap.[key] |> unbox<string>))
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let squareRosetta: string array array = [|[|"A"; "B"; "C"; "D"; "E"|]; [|"F"; "G"; "H"; "I"; "K"|]; [|"L"; "M"; "N"; "O"; "P"|]; [|"Q"; "R"; "S"; "T"; "U"|]; [|"V"; "W"; "X"; "Y"; "Z"|]; [|"J"; "1"; "2"; "3"; "4"|]|]
        let squareWikipedia: string array array = [|[|"B"; "G"; "W"; "K"; "Z"|]; [|"Q"; "P"; "N"; "D"; "S"|]; [|"I"; "O"; "A"; "X"; "E"|]; [|"F"; "C"; "L"; "U"; "M"|]; [|"T"; "H"; "Y"; "V"; "R"|]; [|"J"; "1"; "2"; "3"; "4"|]|]
        let textRosetta: string = "0ATTACKATDAWN"
        let textWikipedia: string = "FLEEATONCE"
        let textTest: string = "The invasion will start on the first of January"
        let mutable maps: Map<string, obj> = square_to_maps squareRosetta
        let mutable emap: obj = maps.["e"]
        let mutable dmap: obj = maps.["d"]
        printfn "%s" "from Rosettacode"
        printfn "%s" ("original:\t " + textRosetta)
        let mutable s: string = encrypt textRosetta (unbox<Map<string, int array>> emap) (unbox<Map<string, string>> dmap)
        printfn "%s" ("codiert:\t " + s)
        s <- decrypt s (unbox<Map<string, int array>> emap) (unbox<Map<string, string>> dmap)
        printfn "%s" ("and back:\t " + s)
        maps <- square_to_maps squareWikipedia
        emap <- maps.["e"]
        dmap <- maps.["d"]
        printfn "%s" "from Wikipedia"
        printfn "%s" ("original:\t " + textWikipedia)
        s <- encrypt textWikipedia (unbox<Map<string, int array>> emap) (unbox<Map<string, string>> dmap)
        printfn "%s" ("codiert:\t " + s)
        s <- decrypt s (unbox<Map<string, int array>> emap) (unbox<Map<string, string>> dmap)
        printfn "%s" ("and back:\t " + s)
        maps <- square_to_maps squareWikipedia
        emap <- maps.["e"]
        dmap <- maps.["d"]
        printfn "%s" "from Rosettacode long part"
        printfn "%s" ("original:\t " + textTest)
        s <- encrypt textTest (unbox<Map<string, int array>> emap) (unbox<Map<string, string>> dmap)
        printfn "%s" ("codiert:\t " + s)
        s <- decrypt s (unbox<Map<string, int array>> emap) (unbox<Map<string, string>> dmap)
        printfn "%s" ("and back:\t " + s)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
