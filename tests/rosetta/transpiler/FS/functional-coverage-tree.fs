// Generated 2025-08-02 00:57 +0700

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

let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable r: float = 1.0
        let mutable i: int = 0
        while i < n do
            r <- r * 10.0
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and formatFloat (f: float) (prec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    let mutable prec = prec
    try
        let scale: float = pow10 prec
        let scaled: float = (f * scale) + 0.5
        let mutable n: int = int scaled
        let mutable digits: string = string n
        while (String.length digits) <= prec do
            digits <- "0" + digits
        let intPart: string = _substring digits 0 ((String.length digits) - prec)
        let fracPart: string = _substring digits ((String.length digits) - prec) (String.length digits)
        __ret <- (intPart + ".") + fracPart
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable res: string = ""
        let mutable n: int = w - (String.length s)
        while n > 0 do
            res <- res + " "
            n <- n - 1
        __ret <- res + s
        raise Return
        __ret
    with
        | Return -> __ret
and repeat (ch: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable n = n
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < n do
            s <- s + ch
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and toFloat (i: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable i = i
    try
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
and newNode (name: string) (weight: int) (coverage: float) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable name = name
    let mutable weight = weight
    let mutable coverage = coverage
    try
        __ret <- unbox<Map<string, obj>> (Map.ofList [("name", box name); ("weight", box weight); ("coverage", box coverage); ("children", box [||])])
        raise Return
        __ret
    with
        | Return -> __ret
and addChildren (n: Map<string, obj>) (nodes: Map<string, obj> array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable nodes = nodes
    try
        let mutable cs: obj array = unbox<obj array> (n.["children"])
        for node in nodes do
            cs <- Array.append cs [|node|]
        n <- Map.add "children" (box cs) n
        __ret
    with
        | Return -> __ret
and setCoverage (n: Map<string, obj>) (value: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable value = value
    try
        n <- Map.add "coverage" (box value) n
        __ret
    with
        | Return -> __ret
and computeCoverage (n: Map<string, obj>) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable cs: obj array = unbox<obj array> (n.["children"])
        if (Seq.length cs) = 0 then
            __ret <- unbox<float> (n.["coverage"])
            raise Return
        let mutable v1: float = 0.0
        let mutable v2: int = 0
        for node in cs do
            let m: Map<string, obj> = unbox<Map<string, obj>> node
            let c: float = computeCoverage m
            v1 <- v1 + (float ((float (toFloat (unbox<int> (m.["weight"])))) * c))
            v2 <- v2 + (unbox<int> (m.["weight"]))
        __ret <- v1 / (float (toFloat v2))
        raise Return
        __ret
    with
        | Return -> __ret
and spaces (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- repeat " " n
        raise Return
        __ret
    with
        | Return -> __ret
and show (n: Map<string, obj>) (level: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable level = level
    try
        let mutable indent: int = level * 4
        let name: string = unbox<string> (n.["name"])
        let mutable nl: int = (String.length name) + indent
        let mutable line: string = (unbox<string> (spaces indent)) + name
        line <- (line + (unbox<string> (spaces (32 - nl)))) + "|  "
        line <- (line + (unbox<string> (padLeft (string (unbox<int> (n.["weight"]))) 3))) + "   | "
        line <- (line + (unbox<string> (formatFloat (computeCoverage n) 6))) + " |"
        printfn "%s" line
        let mutable cs: obj array = unbox<obj array> (n.["children"])
        for child in cs do
            show (unbox<Map<string, obj>> child) (level + 1)
        __ret
    with
        | Return -> __ret
let mutable house1: Map<string, obj> = newNode "house1" 40 0.0
let mutable house2: Map<string, obj> = newNode "house2" 60 0.0
let mutable h1_bedrooms: Map<string, obj> = newNode "bedrooms" 1 0.25
let mutable h1_bathrooms: Map<string, obj> = newNode "bathrooms" 1 0.0
let mutable h1_attic: Map<string, obj> = newNode "attic" 1 0.75
let mutable h1_kitchen: Map<string, obj> = newNode "kitchen" 1 0.1
let mutable h1_living_rooms: Map<string, obj> = newNode "living_rooms" 1 0.0
let mutable h1_basement: Map<string, obj> = newNode "basement" 1 0.0
let mutable h1_garage: Map<string, obj> = newNode "garage" 1 0.0
let mutable h1_garden: Map<string, obj> = newNode "garden" 1 0.8
let mutable h2_upstairs: Map<string, obj> = newNode "upstairs" 1 0.0
let mutable h2_groundfloor: Map<string, obj> = newNode "groundfloor" 1 0.0
let mutable h2_basement: Map<string, obj> = newNode "basement" 1 0.0
let mutable h1_bathroom1: Map<string, obj> = newNode "bathroom1" 1 0.5
let mutable h1_bathroom2: Map<string, obj> = newNode "bathroom2" 1 0.0
let mutable h1_outside: Map<string, obj> = newNode "outside_lavatory" 1 1.0
let mutable h1_lounge: Map<string, obj> = newNode "lounge" 1 0.0
let mutable h1_dining: Map<string, obj> = newNode "dining_room" 1 0.0
let mutable h1_conservatory: Map<string, obj> = newNode "conservatory" 1 0.0
let mutable h1_playroom: Map<string, obj> = newNode "playroom" 1 1.0
let mutable h2_bedrooms: Map<string, obj> = newNode "bedrooms" 1 0.0
let mutable h2_bathroom: Map<string, obj> = newNode "bathroom" 1 0.0
let mutable h2_toilet: Map<string, obj> = newNode "toilet" 1 0.0
let mutable h2_attics: Map<string, obj> = newNode "attics" 1 0.6
let mutable h2_kitchen: Map<string, obj> = newNode "kitchen" 1 0.0
let mutable h2_living_rooms: Map<string, obj> = newNode "living_rooms" 1 0.0
let mutable h2_wet_room: Map<string, obj> = newNode "wet_room_&_toilet" 1 0.0
let mutable h2_garage: Map<string, obj> = newNode "garage" 1 0.0
let mutable h2_garden: Map<string, obj> = newNode "garden" 1 0.9
let mutable h2_hot_tub: Map<string, obj> = newNode "hot_tub_suite" 1 1.0
let mutable h2_cellars: Map<string, obj> = newNode "cellars" 1 1.0
let mutable h2_wine_cellar: Map<string, obj> = newNode "wine_cellar" 1 1.0
let mutable h2_cinema: Map<string, obj> = newNode "cinema" 1 0.75
let mutable h2_suite1: Map<string, obj> = newNode "suite_1" 1 0.0
let mutable h2_suite2: Map<string, obj> = newNode "suite_2" 1 0.0
let mutable h2_bedroom3: Map<string, obj> = newNode "bedroom_3" 1 0.0
let mutable h2_bedroom4: Map<string, obj> = newNode "bedroom_4" 1 0.0
let mutable h2_lounge: Map<string, obj> = newNode "lounge" 1 0.0
let mutable h2_dining: Map<string, obj> = newNode "dining_room" 1 0.0
let mutable h2_conservatory: Map<string, obj> = newNode "conservatory" 1 0.0
let mutable h2_playroom: Map<string, obj> = newNode "playroom" 1 0.0
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable cleaning: Map<string, obj> = newNode "cleaning" 1 0.0
        addChildren h1_bathrooms [|h1_bathroom1; h1_bathroom2; h1_outside|]
        addChildren h1_living_rooms [|h1_lounge; h1_dining; h1_conservatory; h1_playroom|]
        addChildren house1 [|h1_bedrooms; h1_bathrooms; h1_attic; h1_kitchen; h1_living_rooms; h1_basement; h1_garage; h1_garden|]
        addChildren h2_bedrooms [|h2_suite1; h2_suite2; h2_bedroom3; h2_bedroom4|]
        addChildren h2_upstairs [|h2_bedrooms; h2_bathroom; h2_toilet; h2_attics|]
        addChildren h2_living_rooms [|h2_lounge; h2_dining; h2_conservatory; h2_playroom|]
        addChildren h2_groundfloor [|h2_kitchen; h2_living_rooms; h2_wet_room; h2_garage; h2_garden; h2_hot_tub|]
        addChildren h2_basement [|h2_cellars; h2_wine_cellar; h2_cinema|]
        addChildren house2 [|h2_upstairs; h2_groundfloor; h2_basement|]
        addChildren cleaning [|house1; house2|]
        let topCoverage: float = computeCoverage cleaning
        printfn "%s" ("TOP COVERAGE = " + (unbox<string> (formatFloat topCoverage 6)))
        printfn "%s" ""
        printfn "%s" "NAME HIERARCHY                 | WEIGHT | COVERAGE |"
        show cleaning 0
        setCoverage h2_cinema 1.0
        let diff: float = (float (computeCoverage cleaning)) - topCoverage
        printfn "%s" ""
        printfn "%s" "If the coverage of the Cinema node were increased from 0.75 to 1"
        printfn "%s" ((("the top level coverage would increase by " + (unbox<string> (formatFloat diff 6))) + " to ") + (unbox<string> (formatFloat (topCoverage + diff) 6)))
        setCoverage h2_cinema 0.75
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
