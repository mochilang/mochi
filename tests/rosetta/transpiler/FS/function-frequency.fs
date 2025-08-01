// Generated 2025-08-01 22:40 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec join (xs: string array) (sep: string) =
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
and sortPairs (xs: Map<string, obj> array) =
    let mutable __ret : Map<string, obj> array = Unchecked.defaultof<Map<string, obj> array>
    let mutable xs = xs
    try
        let mutable arr: Map<string, obj> array = xs
        let mutable i: int = 1
        while i < (Seq.length arr) do
            let mutable j: int = i
            while (j > 0) && ((unbox<int> (arr.[j - 1].["count"])) < (unbox<int> (arr.[j].["count"]))) do
                let tmp: Map<string, obj> = arr.[j - 1]
                arr.[j - 1] <- arr.[j]
                arr.[j] <- tmp
                j <- j - 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and isAlphaNumDot (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (((((ch >= "A") && (ch <= "Z")) || ((ch >= "a") && (ch <= "z"))) || ((ch >= "0") && (ch <= "9"))) || (ch = "_")) || (ch = ".")
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let srcLines: string array = [|"package main"; ""; "import ("; "    \"fmt\""; "    \"go/ast\""; "    \"go/parser\""; "    \"go/token\""; "    \"io/ioutil\""; "    \"os\""; "    \"sort\""; ")"; ""; "func main() {"; "    if len(os.Args) != 2 {"; "        fmt.Println(\"usage ff <go source filename>\")"; "        return"; "    }"; "    src, err := ioutil.ReadFile(os.Args[1])"; "    if err != nil {"; "        fmt.Println(err)"; "        return"; "    }"; "    fs := token.NewFileSet()"; "    a, err := parser.ParseFile(fs, os.Args[1], src, 0)"; "    if err != nil {"; "        fmt.Println(err)"; "        return"; "    }"; "    f := fs.File(a.Pos())"; "    m := make(map[string]int)"; "    ast.Inspect(a, func(n ast.Node) bool {"; "        if ce, ok := n.(*ast.CallExpr); ok {"; "            start := f.Offset(ce.Pos())"; "            end := f.Offset(ce.Lparen)"; "            m[string(src[start:end])]++"; "        }"; "        return true"; "    })"; "    cs := make(calls, 0, len(m))"; "    for k, v := range m {"; "        cs = append(cs, &call{k, v})"; "    }"; "    sort.Sort(cs)"; "    for i, c := range cs {"; "        fmt.Printf(\"%-20s %4d\\n\", c.expr, c.count)"; "        if i == 9 {"; "            break"; "        }"; "    }"; "}"; ""; "type call struct {"; "    expr  string"; "    count int"; "}"; "type calls []*call"; ""; "func (c calls) Len() int           { return len(c) }"; "func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }"; "func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }"|]
        let src: string = join srcLines "\n"
        let mutable freq: Map<string, int> = Map.ofList []
        let mutable i: int = 0
        let mutable order: string array = [||]
        try
            while i < (String.length src) do
                try
                    let ch: string = _substring src i (i + 1)
                    if (((ch >= "A") && (ch <= "Z")) || ((ch >= "a") && (ch <= "z"))) || (ch = "_") then
                        let mutable j: int = i + 1
                        while (j < (String.length src)) && (unbox<bool> (isAlphaNumDot (_substring src j (j + 1)))) do
                            j <- j + 1
                        let token: string = _substring src i j
                        let mutable k: int = j
                        try
                            while k < (String.length src) do
                                try
                                    let cc: string = _substring src k (k + 1)
                                    if (((cc = " ") || (cc = "\t")) || (cc = "\n")) || (cc = "\r") then
                                        k <- k + 1
                                    else
                                        raise Break
                                with
                                | Continue -> ()
                                | Break -> raise Break
                        with
                        | Break -> ()
                        | Continue -> ()
                        if (k < (String.length src)) && ((_substring src k (k + 1)) = "(") then
                            let mutable p: int = i - 1
                            while (p >= 0) && (((_substring src p (p + 1)) = " ") || ((_substring src p (p + 1)) = "\t")) do
                                p <- p - 1
                            let mutable skip: bool = false
                            if p >= 3 then
                                let before: string = _substring src (p - 3) (p + 1)
                                if before = "func" then
                                    skip <- true
                            if not skip then
                                if Map.containsKey token freq then
                                    freq <- Map.add token ((int (freq.[token])) + 1) freq
                                else
                                    freq <- Map.add token 1 freq
                                    order <- Array.append order [|token|]
                        i <- j
                    else
                        i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable pairs: Map<string, obj> array = [||]
        for t in order do
            pairs <- Array.append pairs [|Map.ofList [("expr", box t); ("count", box (freq.[t]))]|]
        pairs <- sortPairs pairs
        let mutable idx: int = 0
        while (idx < (Seq.length pairs)) && (idx < 10) do
            let mutable p: Map<string, obj> = pairs.[idx]
            printfn "%s" (((unbox<string> (p.["expr"])) + " ") + (string (p.["count"])))
            idx <- idx + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
