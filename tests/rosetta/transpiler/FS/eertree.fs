// Generated 2025-07-28 10:03 +0700

exception Break
exception Continue

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
let EVEN_ROOT: int = 0
let ODD_ROOT: int = 1
let rec newNode (len: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable len = len
    try
        __ret <- unbox<Map<string, obj>> (Map.ofList [("length", box len); ("edges", box (Map.ofList [])); ("suffix", box 0)])
        raise Return
        __ret
    with
        | Return -> __ret
and eertree (s: string) =
    let mutable __ret : Map<string, obj> array = Unchecked.defaultof<Map<string, obj> array>
    let mutable s = s
    try
        let mutable tree: Map<string, obj> array = [||]
        tree <- Array.append tree [|Map.ofList [("length", box 0); ("suffix", box ODD_ROOT); ("edges", box (Map.ofList []))]|]
        tree <- Array.append tree [|Map.ofList [("length", box (-1)); ("suffix", box ODD_ROOT); ("edges", box (Map.ofList []))]|]
        let mutable suffix: int = ODD_ROOT
        let mutable i: int = 0
        try
            while i < (String.length s) do
                try
                    let c: string = s.Substring(i, (i + 1) - i)
                    let mutable n: int = suffix
                    let mutable k: int = 0
                    try
                        while true do
                            try
                                k <- unbox<int> (tree.[n].["length"])
                                let b: int = (i - k) - 1
                                if (b >= 0) && ((s.Substring(b, (b + 1) - b)) = c) then
                                    raise Break
                                n <- unbox<int> (tree.[n].["suffix"])
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    let mutable edges: Map<string, int> = unbox<Map<string, int>> (tree.[n].["edges"])
                    if Map.containsKey c edges then
                        suffix <- int (edges.[c] |> unbox<int>)
                        i <- i + 1
                        raise Continue
                    suffix <- Seq.length tree
                    tree <- Array.append tree [|unbox<Map<string, obj>> (newNode (k + 2))|]
                    edges <- Map.add c suffix edges
                    tree <- Map.add n (box (Map.add "edges" (box edges) (tree.[n]))) tree
                    if (unbox<int> (tree.[suffix].["length"])) = 1 then
                        tree <- Map.add suffix (box (Map.add "suffix" (box 0) (tree.[suffix]))) tree
                        i <- i + 1
                        raise Continue
                    try
                        while true do
                            try
                                n <- unbox<int> (tree.[n].["suffix"])
                                let b: int = (i - (unbox<int> (tree.[n].["length"]))) - 1
                                if (b >= 0) && ((s.Substring(b, (b + 1) - b)) = c) then
                                    raise Break
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    let mutable en: Map<string, int> = unbox<Map<string, int>> (tree.[n].["edges"])
                    tree <- Map.add suffix (box (Map.add "suffix" (box (en.[c] |> unbox<int>)) (tree.[suffix]))) tree
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- tree
        raise Return
        __ret
    with
        | Return -> __ret
and child (tree: Map<string, obj> array) (idx: int) (p: string) (acc: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable tree = tree
    let mutable idx = idx
    let mutable p = p
    let mutable acc = acc
    try
        let mutable edges: Map<string, int> = unbox<Map<string, int>> (tree.[idx].["edges"])
        for KeyValue(ch, _) in edges do
            let nxt: int = edges.[ch] |> unbox<int>
            let pal: string = (ch + p) + ch
            acc <- Array.append acc [|pal|]
            acc <- child tree nxt pal acc
        __ret <- acc
        raise Return
        __ret
    with
        | Return -> __ret
and subPalindromes (tree: Map<string, obj> array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable tree = tree
    try
        let mutable res: string array = [||]
        res <- child tree EVEN_ROOT "" res
        let mutable oEdges: Map<string, int> = unbox<Map<string, int>> (tree.[ODD_ROOT].["edges"])
        for KeyValue(ch, _) in oEdges do
            res <- Array.append res [|ch|]
            res <- child tree (int (oEdges.[ch] |> unbox<int>)) ch res
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
        let tree: Map<string, obj> array = eertree "eertree"
        let subs: string array = subPalindromes tree
        printfn "%s" (string subs)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
