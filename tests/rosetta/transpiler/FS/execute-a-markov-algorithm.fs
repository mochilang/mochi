// Generated 2025-08-04 16:14 +0700

exception Break
exception Continue

exception Return
let mutable __ret = ()

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

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
open System.Collections.Generic

let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable parts: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (((String.length (sep)) > 0) && ((i + (String.length (sep))) <= (String.length (s)))) && ((_substring s i (i + (String.length (sep)))) = sep) then
                parts <- Array.append parts [|cur|]
                cur <- ""
                i <- i + (String.length (sep))
            else
                cur <- cur + (_substring s i (i + 1))
                i <- i + 1
        parts <- Array.append parts [|cur|]
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and trimSpace (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable start: int = 0
        while (start < (String.length (s))) && (((s.Substring(start, (start + 1) - start)) = " ") || ((s.Substring(start, (start + 1) - start)) = "\t")) do
            start <- start + 1
        let mutable ``end``: int = String.length (s)
        while (``end`` > start) && (((s.Substring(``end`` - 1, ``end`` - (``end`` - 1))) = " ") || ((s.Substring(``end`` - 1, ``end`` - (``end`` - 1))) = "\t")) do
            ``end`` <- ``end`` - 1
        __ret <- s.Substring(start, ``end`` - start)
        raise Return
        __ret
    with
        | Return -> __ret
and indexOfSub (s: string) (sub: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable sub = sub
    try
        if (String.length (sub)) = 0 then
            __ret <- 0
            raise Return
        let mutable i: int = 0
        while (i + (String.length (sub))) <= (String.length (s)) do
            if (_substring s i (i + (String.length (sub)))) = sub then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and parseRules (rs: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, obj> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, obj>>
    let mutable rs = rs
    try
        let mutable rules: System.Collections.Generic.IDictionary<string, obj> array = [||]
        try
            for line in rs.Split([|"\n"|], System.StringSplitOptions.None) do
                try
                    let mutable ln = line
                    let hash: int = indexOfSub (unbox<string> ln) ("#")
                    if hash >= 0 then
                        ln <- ln.Substring(0, hash - 0)
                    ln <- trimSpace (unbox<string> ln)
                    if (String.length (ln)) = 0 then
                        raise Continue
                    let mutable arrow: int = 0 - 1
                    let mutable j: int = 0
                    try
                        while (j + 2) <= (String.length (ln)) do
                            try
                                if (_substring ln j (j + 2)) = "->" then
                                    let mutable pre: bool = (j > 0) && (((ln.Substring(j - 1, j - (j - 1))) = " ") || ((ln.Substring(j - 1, j - (j - 1))) = "\t"))
                                    let mutable post: bool = ((j + 2) < (String.length (ln))) && (((ln.Substring(j + 2, (j + 3) - (j + 2))) = " ") || ((ln.Substring(j + 2, (j + 3) - (j + 2))) = "\t"))
                                    if pre && post then
                                        arrow <- j
                                        raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if arrow < 0 then
                        arrow <- indexOfSub (ln) ("->")
                    if arrow < 0 then
                        __ret <- unbox<System.Collections.Generic.IDictionary<string, obj>> (_dictCreate [("ok", false)])
                        raise Return
                    let mutable pat: string = trimSpace (ln.Substring(0, arrow - 0))
                    let mutable rest: string = trimSpace (ln.Substring(arrow + 2, (String.length (ln)) - (arrow + 2)))
                    let mutable term: bool = false
                    if ((String.length (rest)) > 0) && ((rest.Substring(0, 1 - 0)) = ".") then
                        term <- true
                        rest <- rest.Substring(1, (String.length (rest)) - 1)
                    let mutable rep: string = rest
                    rules <- Array.append rules [|_dictCreate [("pat", box (pat)); ("rep", box (rep)); ("term", box (term))]|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- unbox<System.Collections.Generic.IDictionary<string, obj>> (_dictCreate [("ok", box (true)); ("rules", box (rules))])
        raise Return
        __ret
    with
        | Return -> __ret
and runRules (rules: System.Collections.Generic.IDictionary<string, obj> array) (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable rules = rules
    let mutable s = s
    try
        let mutable changed: bool = true
        try
            while changed do
                try
                    changed <- false
                    let mutable i: int = 0
                    try
                        while i < (Seq.length (rules)) do
                            try
                                let r: System.Collections.Generic.IDictionary<string, obj> = _idx rules i
                                let pat: obj = r.[(string ("pat"))]
                                let rep: obj = r.[(string ("rep"))]
                                let mutable term: obj = r.[(string ("term"))]
                                let idx: int = indexOfSub (s) (unbox<string> pat)
                                if idx >= 0 then
                                    s <- ((s.Substring(0, idx - 0)) + (unbox<string> rep)) + (s.Substring(idx + (String.length (unbox<string> pat)), (String.length (s)) - (idx + (String.length (unbox<string> pat)))))
                                    changed <- true
                                    if unbox<bool> term then
                                        __ret <- s
                                        raise Return
                                    raise Break
                                i <- i + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and interpret (ruleset: string) (input: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, obj> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, obj>>
    let mutable ruleset = ruleset
    let mutable input = input
    try
        let p: System.Collections.Generic.IDictionary<string, obj> = parseRules (ruleset)
        if not (unbox<bool> (p.[(string ("ok"))])) then
            __ret <- unbox<System.Collections.Generic.IDictionary<string, obj>> (_dictCreate [("ok", box (false)); ("out", box (""))])
            raise Return
        let out: string = runRules (unbox<System.Collections.Generic.IDictionary<string, obj> array> (p.[(string ("rules"))])) (input)
        __ret <- unbox<System.Collections.Generic.IDictionary<string, obj>> (_dictCreate [("ok", box (true)); ("out", box (out))])
        raise Return
        __ret
    with
        | Return -> __ret
let mutable testSet: System.Collections.Generic.IDictionary<string, string> array = [|_dictCreate [("ruleSet", "# This rules file is extracted from Wikipedia:\n# http://en.wikipedia.org/wiki/Markov_Algorithm\nA -> apple\nB -> bag\nS -> shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule\n"); ("sample", "I bought a B of As from T S."); ("output", "I bought a bag of apples from my brother.")]; _dictCreate [("ruleSet", "# Slightly modified from the rules on Wikipedia\nA -> apple\nB -> bag\nS -> .shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule\n"); ("sample", "I bought a B of As from T S."); ("output", "I bought a bag of apples from T shop.")]; _dictCreate [("ruleSet", "# BNF Syntax testing rules\nA -> apple\nWWWW -> with\nBgage -> ->.*\nB -> bag\n->.* -> money\nW -> WW\nS -> .shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule\n"); ("sample", "I bought a B of As W my Bgage from T S."); ("output", "I bought a bag of apples with my money from T shop.")]; _dictCreate [("ruleSet", "### Unary Multiplication Engine, for testing Markov Algorithm implementations\n### By Donal Fellows.\n# Unary addition engine\n_+1 -> _1+\n1+1 -> 11+\n# Pass for converting from the splitting of multiplication into ordinary\n# addition\n1! -> !1\n,! -> !+\n_! -> _\n# Unary multiplication by duplicating left side, right side times\n1*1 -> x,@y\n1x -> xX\nX, -> 1,1\nX1 -> 1X\n_x -> _X\n,x -> ,X\ny1 -> 1y\ny_ -> _\n# Next phase of applying\n1@1 -> x,@y\n1@_ -> @_\n,@_ -> !_\n++ -> +\n# Termination cleanup for addition\n_1 -> 1\n1+_ -> 1\n_+_ ->\n"); ("sample", "_1111*11111_"); ("output", "11111111111111111111")]; _dictCreate [("ruleSet", "# Turing machine: three-state busy beaver\n#\n# state A, symbol 0 => write 1, move right, new state B\nA0 -> 1B\n# state A, symbol 1 => write 1, move left, new state C\n0A1 -> C01\n1A1 -> C11\n# state B, symbol 0 => write 1, move left, new state A\n0B0 -> A01\n1B0 -> A11\n# state B, symbol 1 => write 1, move right, new state B\nB1 -> 1B\n# state C, symbol 0 => write 1, move left, new state B\n0C0 -> B01\n1C0 -> B11\n# state C, symbol 1 => write 1, move left, halt\n0C1 -> H01\n1C1 -> H11\n"); ("sample", "000000A000000"); ("output", "00011H1111000")]|]
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (("validating " + (string (Seq.length (testSet)))) + " test cases")
        let mutable failures: bool = false
        let mutable i: int = 0
        while i < (Seq.length (testSet)) do
            let tc: System.Collections.Generic.IDictionary<string, string> = _idx testSet i
            let res: System.Collections.Generic.IDictionary<string, obj> = interpret (tc.[(string ("ruleSet"))]) (tc.[(string ("sample"))])
            if not (unbox<bool> (res.[(string ("ok"))])) then
                printfn "%s" (("test " + (string (i + 1))) + " invalid ruleset")
                failures <- true
            else
                if (unbox<string> (res.[(string ("out"))])) <> (tc.[(string ("output"))]) then
                    printfn "%s" ((((("test " + (string (i + 1))) + ": got ") + (unbox<string> (res.[(string ("out"))]))) + ", want ") + (tc.[(string ("output"))]))
                    failures <- true
            i <- i + 1
        if not failures then
            printfn "%s" ("no failures")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
