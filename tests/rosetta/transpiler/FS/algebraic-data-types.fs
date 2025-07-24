// Generated 2025-07-24 20:52 +0700

exception Return

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
and treeString (t: obj) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable t = t
    try
        if t = null then
            __ret <- "E"
            raise Return
        let m = t :?> Map<string, obj>
        __ret <- ((((((("T(" + (m.["cl"])) + ", ") + (treeString (m.["le"]))) + ", ") + (string (m.["aa"]))) + ", ") + (treeString (m.["ri"]))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
and balance (t: obj) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable t = t
    try
        if t = null then
            __ret <- t
            raise Return
        let m = t :?> Map<string, obj>
        if (m.["cl"]) <> "B" then
            __ret <- t
            raise Return
        let le = m.["le"]
        let ri = m.["ri"]
        if le <> null then
            let leMap = le :?> Map<string, obj>
            if (leMap.["cl"]) = "R" then
                let lele = leMap.["le"]
                if lele <> null then
                    let leleMap = lele :?> Map<string, obj>
                    if (leleMap.["cl"]) = "R" then
                        __ret <- node "R" (node "B" (leleMap.["le"]) (leleMap.["aa"]) (leleMap.["ri"])) (leMap.["aa"]) (node "B" (leMap.["ri"]) (m.["aa"]) ri)
                        raise Return
                let leri = leMap.["ri"]
                if leri <> null then
                    let leriMap = leri :?> Map<string, obj>
                    if (leriMap.["cl"]) = "R" then
                        __ret <- node "R" (node "B" (leMap.["le"]) (leMap.["aa"]) (leriMap.["le"])) (leriMap.["aa"]) (node "B" (leriMap.["ri"]) (m.["aa"]) ri)
                        raise Return
        if ri <> null then
            let riMap = ri :?> Map<string, obj>
            if (riMap.["cl"]) = "R" then
                let rile = riMap.["le"]
                if rile <> null then
                    let rileMap = rile :?> Map<string, obj>
                    if (rileMap.["cl"]) = "R" then
                        __ret <- node "R" (node "B" (m.["le"]) (m.["aa"]) (rileMap.["le"])) (rileMap.["aa"]) (node "B" (rileMap.["ri"]) (riMap.["aa"]) (riMap.["ri"]))
                        raise Return
                let riri = riMap.["ri"]
                if riri <> null then
                    let ririMap = riri :?> Map<string, obj>
                    if (ririMap.["cl"]) = "R" then
                        __ret <- node "R" (node "B" (m.["le"]) (m.["aa"]) (riMap.["le"])) (riMap.["aa"]) (node "B" (ririMap.["le"]) (ririMap.["aa"]) (ririMap.["ri"]))
                        raise Return
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
and ins (tr: obj) (x: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable tr = tr
    let mutable x = x
    try
        if tr = null then
            __ret <- node "R" null x null
            raise Return
        if x < (tr.["aa"]) then
            __ret <- balance (node (tr.["cl"]) (ins (tr.["le"]) x) (tr.["aa"]) (tr.["ri"]))
            raise Return
        if x > (tr.["aa"]) then
            __ret <- balance (node (tr.["cl"]) (tr.["le"]) (tr.["aa"]) (ins (tr.["ri"]) x))
            raise Return
        __ret <- tr
        raise Return
        __ret
    with
        | Return -> __ret
and insert (tr: obj) (x: int) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable tr = tr
    let mutable x = x
    try
        let t = ins tr x
        if t = null then
            __ret <- null
            raise Return
        let m = t :?> Map<string, obj>
        __ret <- node "B" (m.["le"]) (m.["aa"]) (m.["ri"])
        raise Return
        __ret
    with
        | Return -> __ret
let mutable tr: obj = null
let mutable i: int = 1
while i <= 16 do
    tr <- insert tr i
    i <- i + 1
printfn "%A" (treeString tr)
