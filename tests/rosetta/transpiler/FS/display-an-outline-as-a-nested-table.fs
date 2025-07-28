// Generated 2025-07-28 10:03 +0700

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

let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable out: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            if ((i + (String.length sep)) <= (String.length s)) && ((_substring s i (i + (String.length sep))) = sep) then
                out <- Array.append out [|cur|]
                cur <- ""
                i <- i + (String.length sep)
            else
                cur <- cur + (_substring s i (i + 1))
                i <- i + 1
        out <- Array.append out [|cur|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and join (xs: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    let mutable sep = sep
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length xs) do
            if i > 0 then
                res <- res + sep
            res <- res + (xs.[i])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and trimLeftSpaces (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable i: int = 0
        while (i < (String.length s)) && ((s.Substring(i, (i + 1) - i)) = " ") do
            i <- i + 1
        __ret <- s.Substring(i, (String.length s) - i)
        raise Return
        __ret
    with
        | Return -> __ret
and makeIndent (outline: string) (tab: int) =
    let mutable __ret : Map<string, obj> array = Unchecked.defaultof<Map<string, obj> array>
    let mutable outline = outline
    let mutable tab = tab
    try
        let lines: string array = Array.toList (outline.Split([|"\n"|], System.StringSplitOptions.None))
        let mutable nodes: Map<string, obj> array = [||]
        for line in lines do
            let line2: string = trimLeftSpaces line
            let level: int = ((String.length line) - (String.length line2)) / tab
            nodes <- Array.append nodes [|Map.ofList [("level", box level); ("name", box line2)]|]
        __ret <- nodes
        raise Return
        __ret
    with
        | Return -> __ret
and toNest (nodes: Map<string, obj> array) (start: int) (level: int) (n: Map<string, obj>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable nodes = nodes
    let mutable start = start
    let mutable level = level
    let mutable n = n
    try
        if level = 0 then
            n <- Map.add "name" (nodes.[0].["name"]) n
        let mutable i: int = start + 1
        while i < (Seq.length nodes) do
            let node: Map<string, obj> = nodes.[i]
            let lev: int = int (node.["level"])
            if lev = (level + 1) then
                let mutable child = Map.ofList [("name", box (node.["name"])); ("children", box [||])]
                toNest nodes i (level + 1) child
                let mutable cs: obj array = unbox<obj array> (n.["children"])
                cs <- Array.append cs [|child|]
                n <- Map.add "children" (box cs) n
            else
                if lev <= level then
                    __ret <- ()
                    raise Return
            i <- i + 1
        __ret
    with
        | Return -> __ret
and countLeaves (n: Map<string, obj>) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let kids: obj array = unbox<obj array> (n.["children"])
        if (Seq.length kids) = 0 then
            __ret <- 1
            raise Return
        let mutable total: int = 0
        for k in kids do
            total <- total + (int (countLeaves (unbox<Map<string, obj>> k)))
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and nodesByDepth (root: Map<string, obj>) (depth: int) =
    let mutable __ret : Map<string, obj> array array = Unchecked.defaultof<Map<string, obj> array array>
    let mutable root = root
    let mutable depth = depth
    try
        let mutable levels: Map<string, obj> array array = [||]
        let mutable current: Map<string, obj> array = [|root|]
        let mutable d: int = 0
        while d < depth do
            levels <- Array.append levels [|current|]
            let mutable next: Map<string, obj> array = [||]
            for n in current do
                let kids: obj array = unbox<obj array> (n.["children"])
                for k in kids do
                    next <- Array.append next [|unbox<Map<string, obj>> k|]
            current <- next
            d <- d + 1
        __ret <- levels
        raise Return
        __ret
    with
        | Return -> __ret
and toMarkup (n: Map<string, obj>) (cols: string array) (depth: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable cols = cols
    let mutable depth = depth
    try
        let mutable lines: string array = [||]
        lines <- Array.append lines [|"{| class=\"wikitable\" style=\"text-align: center;\""|]
        let l1: string = "|-"
        lines <- Array.append lines [|l1|]
        let span: int = countLeaves n
        lines <- Array.append lines [|(((("| style=\"background: " + (cols.[0])) + " \" colSpan=") + (string span)) + " | ") + (unbox<string> (n.["name"]))|]
        lines <- Array.append lines [|l1|]
        let lvls: Map<string, obj> array array = nodesByDepth n depth
        let mutable lvl: int = 1
        while lvl < depth do
            let nodes: Map<string, obj> array = lvls.[lvl]
            if (Seq.length nodes) = 0 then
                lines <- Array.append lines [|"|  |"|]
            else
                let mutable idx: int = 0
                while idx < (Seq.length nodes) do
                    let node: Map<string, obj> = nodes.[idx]
                    span <- countLeaves node
                    let mutable col: int = lvl
                    if lvl = 1 then
                        col <- idx + 1
                    if col >= (Seq.length cols) then
                        col <- (Seq.length cols) - 1
                    let cell: string = (((("| style=\"background: " + (cols.[col])) + " \" colspan=") + (string span)) + " | ") + (unbox<string> (node.["name"]))
                    lines <- Array.append lines [|cell|]
                    idx <- idx + 1
            if lvl < (depth - 1) then
                lines <- Array.append lines [|l1|]
            lvl <- lvl + 1
        lines <- Array.append lines [|"|}"|]
        __ret <- join lines "\n"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let outline: string = (((((((((("Display an outline as a nested table.\n" + "    Parse the outline to a tree,\n") + "        measuring the indent of each line,\n") + "        translating the indentation to a nested structure,\n") + "        and padding the tree to even depth.\n") + "    count the leaves descending from each node,\n") + "        defining the width of a leaf as 1,\n") + "        and the width of a parent node as a sum.\n") + "            (The sum of the widths of its children)\n") + "    and write out a table with 'colspan' values\n") + "        either as a wiki table,\n") + "        or as HTML."
        let yellow: string = "#ffffe6;"
        let orange: string = "#ffebd2;"
        let green: string = "#f0fff0;"
        let blue: string = "#e6ffff;"
        let pink: string = "#ffeeff;"
        let cols: string array = [|yellow; orange; green; blue; pink|]
        let nodes: Map<string, obj> array = makeIndent outline 4
        let mutable n = Map.ofList [("name", box ""); ("children", box [||])]
        toNest nodes 0 0 n
        printfn "%s" (toMarkup n cols 4)
        printfn "%s" "\n"
        let outline2: string = (((((((((((("Display an outline as a nested table.\n" + "    Parse the outline to a tree,\n") + "        measuring the indent of each line,\n") + "        translating the indentation to a nested structure,\n") + "        and padding the tree to even depth.\n") + "    count the leaves descending from each node,\n") + "        defining the width of a leaf as 1,\n") + "        and the width of a parent node as a sum.\n") + "            (The sum of the widths of its children)\n") + "            Propagating the sums upward as necessary.\n") + "    and write out a table with 'colspan' values\n") + "        either as a wiki table,\n") + "        or as HTML.\n") + "    Optionally add color to the nodes."
        let cols2: string array = [|blue; yellow; orange; green; pink|]
        let nodes2: Map<string, obj> array = makeIndent outline2 4
        let mutable n2 = Map.ofList [("name", box ""); ("children", box [||])]
        toNest nodes2 0 0 n2
        printfn "%s" (toMarkup n2 cols2 4)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
