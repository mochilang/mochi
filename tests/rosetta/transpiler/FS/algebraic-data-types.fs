// Generated 2025-07-25 17:45 +0700

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
let rec node (cl: string) (le: obj) (aa: int) (ri: obj) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable cl = cl
    let mutable le = le
    let mutable aa = aa
    let mutable ri = ri
    try
        __ret <- Map.ofList [("cl", box cl); ("le", box le); ("aa", box aa); ("ri", box ri)]
        raise Return
        __ret
    with
        | Return -> __ret
let rec treeString (t: obj) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable t = t
    try
        if t = null then
            __ret <- "E"
            raise Return
        let m: Map<string, obj> = unbox<Map<string, obj>> t
        __ret <- ((((((("T(" + (unbox<string> (m.["cl"]))) + ", ") + (unbox<string> (treeString (m.["le"])))) + ", ") + (string (m.["aa"]))) + ", ") + (unbox<string> (treeString (m.["ri"])))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let rec balance (t: obj) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable t = t
    try
        if t = null then
            __ret <- t
            raise Return
        let m: Map<string, obj> = unbox<Map<string, obj>> t
        if (unbox<string> (m.["cl"])) <> "B" then
            __ret <- t
            raise Return
        let le: obj = m.["le"]
        let ri: obj = m.["ri"]
        if le <> null then
            let leMap: Map<string, obj> = unbox<Map<string, obj>> le
            if (unbox<string> (leMap.["cl"])) = "R" then
                let lele: obj = leMap.["le"]
                if lele <> null then
                    let leleMap: Map<string, obj> = unbox<Map<string, obj>> lele
                    if (unbox<string> (leleMap.["cl"])) = "R" then
                        __ret <- node "R" (node "B" (leleMap.["le"]) (unbox<int> (leleMap.["aa"])) (leleMap.["ri"])) (unbox<int> (leMap.["aa"])) (node "B" (leMap.["ri"]) (unbox<int> (m.["aa"])) ri)
                        raise Return
                let leri: obj = leMap.["ri"]
                if leri <> null then
                    let leriMap: Map<string, obj> = unbox<Map<string, obj>> leri
                    if (unbox<string> (leriMap.["cl"])) = "R" then
                        __ret <- node "R" (node "B" (leMap.["le"]) (unbox<int> (leMap.["aa"])) (leriMap.["le"])) (unbox<int> (leriMap.["aa"])) (node "B" (leriMap.["ri"]) (unbox<int> (m.["aa"])) ri)
                        raise Return
        if ri <> null then
            let riMap: Map<string, obj> = unbox<Map<string, obj>> ri
            if (unbox<string> (riMap.["cl"])) = "R" then
                let rile: obj = riMap.["le"]
                if rile <> null then
                    let rileMap: Map<string, obj> = unbox<Map<string, obj>> rile
                    if (unbox<string> (rileMap.["cl"])) = "R" then
                        __ret <- node "R" (node "B" (m.["le"]) (unbox<int> (m.["aa"])) (rileMap.["le"])) (unbox<int> (rileMap.["aa"])) (node "B" (rileMap.["ri"]) (unbox<int> (riMap.["aa"])) (riMap.["ri"]))
                        raise Return
                let riri: obj = riMap.["ri"]
                if riri <> null then
                    let ririMap: Map<string, obj> = unbox<Map<string, obj>> riri
                    if (unbox<string> (ririMap.["cl"])) = "R" then
                        __ret <- node "R" (node "B" (m.["le"]) (unbox<int> (m.["aa"])) (riMap.["le"])) (unbox<int> (riMap.["aa"])) (node "B" (ririMap.["le"]) (unbox<int> (ririMap.["aa"])) (ririMap.["ri"]))
                        raise Return
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
let rec ins (tr: obj) (x: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable tr = tr
    let mutable x = x
    try
        if tr = null then
            __ret <- node "R" null x null
            raise Return
        if x < (unbox<int> (((tr :?> Map<string, obj>).["aa"]))) then
            __ret <- balance (node (unbox<string> (((tr :?> Map<string, obj>).["cl"]))) (ins (((tr :?> Map<string, obj>).["le"])) x) (unbox<int> (((tr :?> Map<string, obj>).["aa"]))) (((tr :?> Map<string, obj>).["ri"])))
            raise Return
        if x > (unbox<int> (((tr :?> Map<string, obj>).["aa"]))) then
            __ret <- balance (node (unbox<string> (((tr :?> Map<string, obj>).["cl"]))) (((tr :?> Map<string, obj>).["le"])) (unbox<int> (((tr :?> Map<string, obj>).["aa"]))) (ins (((tr :?> Map<string, obj>).["ri"])) x))
            raise Return
        __ret <- tr
        raise Return
        __ret
    with
        | Return -> __ret
let rec insert (tr: obj) (x: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable tr = tr
    let mutable x = x
    try
        let t = ins tr x
        if t = null then
            __ret <- null
            raise Return
        let m: Map<string, obj> = unbox<Map<string, obj>> t
        __ret <- node "B" (m.["le"]) (unbox<int> (m.["aa"])) (m.["ri"])
        raise Return
        __ret
    with
        | Return -> __ret
let mutable tr: obj = Unchecked.defaultof<obj>
let mutable i: int = 1
while i <= 16 do
    tr <- insert tr i
    i <- i + 1
printfn "%A" (treeString tr)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
