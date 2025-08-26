// Generated 2025-08-26 14:25 +0700

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

let _readLine () =
    match System.Console.ReadLine() with
    | null -> ""
    | s -> s
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    match box v with
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
open System.Collections.Generic

open System

let rec parseIntStr (str: string) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable str = str
    try
        let digits: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        let mutable i: int64 = int64 0
        let mutable n: int64 = int64 0
        while i < (int64 (String.length (str))) do
            n <- (n * (int64 10)) + (int64 (_dictGet digits ((string (_substring str (int i) (int (i + (int64 1))))))))
            i <- i + (int64 1)
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and splitSpaces (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable parts: string array = Array.empty<string>
        let mutable last: int64 = int64 0
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (s))) do
            let ch: string = _substring s (int i) (int (i + (int64 1)))
            if ch = " " then
                parts <- Array.append parts [|(_substring s (int last) (int i))|]
                last <- i + (int64 1)
            if (i + (int64 1)) = (int64 (String.length (s))) then
                parts <- Array.append parts [|(_substring s (int last) (int (i + (int64 1))))|]
            i <- i + (int64 1)
        __ret <- parts
        raise Return
        __ret
    with
        | Return -> __ret
and join (parts: string array) (sep: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable parts = parts
    let mutable sep = sep
    try
        if (Seq.length (parts)) = 0 then
            __ret <- ""
            raise Return
        let mutable res: string = _idx parts (int 0)
        let mutable i: int64 = int64 1
        while i < (int64 (Seq.length (parts))) do
            res <- (res + sep) + (_idx parts (int i))
            i <- i + (int64 1)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and startsWith (s: string) (prefix: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable prefix = prefix
    try
        __ret <- if (String.length (prefix)) > (String.length (s)) then false else ((_substring s 0 (String.length (prefix))) = prefix)
        raise Return
        __ret
    with
        | Return -> __ret
and endsWith (s: string) (suffix: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable suffix = suffix
    try
        __ret <- if (String.length (suffix)) > (String.length (s)) then false else ((_substring s ((String.length (s)) - (String.length (suffix))) (String.length (s))) = suffix)
        raise Return
        __ret
    with
        | Return -> __ret
and ansSubject (sub: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sub = sub
    try
        if sub = "I" then
            __ret <- "you"
            raise Return
        if sub = "you" then
            __ret <- "I"
            raise Return
        __ret <- sub
        raise Return
        __ret
    with
        | Return -> __ret
and conjVerb (sub: string) (verb: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sub = sub
    let mutable verb = verb
    try
        __ret <- if (sub = "I") || (sub = "you") then verb else (verb + "s")
        raise Return
        __ret
    with
        | Return -> __ret
and negWord (sub: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sub = sub
    try
        __ret <- if (sub = "I") || (sub = "you") then "don't" else "doesn't"
        raise Return
        __ret
    with
        | Return -> __ret
and containsStr (lst: string array) (target: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable lst = lst
    let mutable target = target
    try
        for s in Seq.map string (lst) do
            if s = target then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and joinSubjects (lst: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable lst = lst
    try
        if (Seq.length (lst)) = 0 then
            __ret <- ""
            raise Return
        if (Seq.length (lst)) = 1 then
            __ret <- _idx lst (int 0)
            raise Return
        if (Seq.length (lst)) = 2 then
            __ret <- ((_idx lst (int 0)) + " and ") + (_idx lst (int 1))
            raise Return
        let mutable res: string = _idx lst (int 0)
        let mutable i: int64 = int64 1
        while i < (int64 ((Seq.length (lst)) - 1)) do
            res <- (res + ", ") + (_idx lst (int i))
            i <- i + (int64 1)
        res <- (res + ", and ") + (_idx lst (int ((Seq.length (lst)) - 1)))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and actKey (p: string) (o: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    let mutable o = o
    try
        __ret <- (p + "|") + o
        raise Return
        __ret
    with
        | Return -> __ret
and causesContradiction (facts: obj array) (f: obj) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable facts = facts
    let mutable f = f
    try
        for g in facts do
            if ((((((g :?> System.Collections.Generic.IDictionary<string, obj>).["predicate"])) = (((f :?> System.Collections.Generic.IDictionary<string, obj>).["predicate"]))) && ((((g :?> System.Collections.Generic.IDictionary<string, obj>).["object"])) = (((f :?> System.Collections.Generic.IDictionary<string, obj>).["object"])))) && ((((((g :?> System.Collections.Generic.IDictionary<string, obj>).["subject"])) = (((f :?> System.Collections.Generic.IDictionary<string, obj>).["subject"]))) || ((unbox<string> (((g :?> System.Collections.Generic.IDictionary<string, obj>).["subject"]))) = "*")) || ((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["subject"]))) = "*"))) && ((((g :?> System.Collections.Generic.IDictionary<string, obj>).["positive"])) <> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["positive"]))) then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and parseStatement (line: string) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable line = line
    try
        let core: string = _substring line 0 ((String.length (line)) - 1)
        let mutable parts: string array = splitSpaces (core)
        if ((Seq.length (parts)) >= 3) && (((_idx parts (int 1)) = "don't") || ((_idx parts (int 1)) = "doesn't")) then
            let subj: string = _idx parts (int 0)
            let pred: string = _idx parts (int 2)
            let mutable obj: string = ""
            if (Seq.length (parts)) > 3 then
                obj <- join (Array.sub parts 3 ((Seq.length (parts)) - 3)) (" ")
            __ret <- (_dictCreate [("subject", box (subj)); ("predicate", box (pred)); ("object", box (obj)); ("positive", box (false))])
            raise Return
        else
            let subj: string = _idx parts (int 0)
            let predWord: string = _idx parts (int 1)
            let mutable predicate: string = predWord
            if (subj <> "I") && (subj <> "you") then
                predicate <- _substring predWord 0 ((String.length (predWord)) - 1)
            let mutable obj: string = if (Seq.length (parts)) > 2 then (join (Array.sub parts 2 ((Seq.length (parts)) - 2)) (" ")) else ""
            if subj = "everybody" then
                __ret <- (_dictCreate [("subject", box ("*")); ("predicate", box (predicate)); ("object", box (obj)); ("positive", box (true))])
                raise Return
            else
                if subj = "nobody" then
                    __ret <- (_dictCreate [("subject", box ("*")); ("predicate", box (predicate)); ("object", box (obj)); ("positive", box (false))])
                    raise Return
                else
                    __ret <- (_dictCreate [("subject", box (subj)); ("predicate", box (predicate)); ("object", box (obj)); ("positive", box (true))])
                    raise Return
        __ret
    with
        | Return -> __ret
and answerDo (sub: string) (pred: string) (obj: string) (facts: obj array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sub = sub
    let mutable pred = pred
    let mutable obj = obj
    let mutable facts = facts
    try
        let mutable pos: bool = false
        let mutable neg: bool = false
        for f in facts do
            if (((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["predicate"]))) = pred) && ((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["object"]))) = obj)) && (((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["subject"]))) = sub) || ((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["subject"]))) = "*")) then
                if unbox<bool> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["positive"])) then
                    pos <- true
                else
                    neg <- true
        let subA: string = ansSubject (sub)
        let objPart: string = if obj = "" then "" else (" " + obj)
        if pos then
            let verb: string = conjVerb (subA) (pred)
            __ret <- (((("yes, " + subA) + " ") + verb) + objPart) + "."
            raise Return
        if neg then
            let nw: string = negWord (subA)
            __ret <- (((((("no, " + subA) + " ") + nw) + " ") + pred) + objPart) + "."
            raise Return
        __ret <- "maybe."
        raise Return
        __ret
    with
        | Return -> __ret
and answerWho (pred: string) (obj: string) (facts: obj array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable pred = pred
    let mutable obj = obj
    let mutable facts = facts
    try
        let mutable hasEvery: bool = false
        let mutable hasNobody: bool = false
        let mutable subs: string array = Array.empty<string>
        for f in facts do
            if ((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["predicate"]))) = pred) && ((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["object"]))) = obj) then
                if (unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["subject"]))) = "*" then
                    if unbox<bool> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["positive"])) then
                        hasEvery <- true
                    else
                        hasNobody <- true
                else
                    if unbox<bool> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["positive"])) then
                        let sub: string = ansSubject (unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["subject"])))
                        if not (containsStr (subs) (sub)) then
                            subs <- Array.append subs [|sub|]
        let objPart: string = if obj = "" then "" else (" " + obj)
        if hasNobody then
            __ret <- (("nobody " + (conjVerb ("nobody") (pred))) + objPart) + "."
            raise Return
        if hasEvery then
            __ret <- (("everybody " + (conjVerb ("everybody") (pred))) + objPart) + "."
            raise Return
        if (Seq.length (subs)) > 0 then
            let subjStr: string = joinSubjects (subs)
            let verb: string = if (Seq.length (subs)) = 1 then (conjVerb (_idx subs (int 0)) (pred)) else pred
            __ret <- (((subjStr + " ") + verb) + objPart) + "."
            raise Return
        __ret <- "I don't know."
        raise Return
        __ret
    with
        | Return -> __ret
and answerWhat (sub: string) (facts: obj array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable sub = sub
    let mutable facts = facts
    try
        let mutable seen: string array = Array.empty<string>
        let mutable acts: obj array = [||]
        for f in facts do
            if ((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["subject"]))) = sub) || ((unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["subject"]))) = "*") then
                let key: string = actKey (unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["predicate"]))) (unbox<string> (((f :?> System.Collections.Generic.IDictionary<string, obj>).["object"])))
                if not (containsStr (seen) (key)) then
                    seen <- Array.append seen [|key|]
                    acts <- Array.append acts [|box (_dictCreate [("positive", ((f :?> System.Collections.Generic.IDictionary<string, obj>).["positive"])); ("predicate", ((f :?> System.Collections.Generic.IDictionary<string, obj>).["predicate"])); ("object", ((f :?> System.Collections.Generic.IDictionary<string, obj>).["object"]))])|]
        if (Seq.length (acts)) = 0 then
            __ret <- "I don't know."
            raise Return
        let subA: string = ansSubject (sub)
        let mutable phrases: string array = Array.empty<string>
        for a in acts do
            let mutable ``base``: string = ""
            if unbox<bool> (((a :?> System.Collections.Generic.IDictionary<string, obj>).["positive"])) then
                ``base`` <- conjVerb (subA) (unbox<string> (((a :?> System.Collections.Generic.IDictionary<string, obj>).["predicate"])))
            else
                ``base`` <- ((negWord (subA)) + " ") + (_str (((a :?> System.Collections.Generic.IDictionary<string, obj>).["predicate"])))
            if (unbox<string> (((a :?> System.Collections.Generic.IDictionary<string, obj>).["object"]))) <> "" then
                ``base`` <- (``base`` + " ") + (_str (((a :?> System.Collections.Generic.IDictionary<string, obj>).["object"])))
            phrases <- Array.append phrases [|``base``|]
        let mutable res: string = ""
        if (Seq.length (phrases)) = 1 then
            res <- _idx phrases (int 0)
        else
            if (Seq.length (phrases)) = 2 then
                res <- ((_idx phrases (int 0)) + ", and ") + (_idx phrases (int 1))
            else
                res <- _idx phrases (int 0)
                let mutable i: int64 = int64 1
                while i < (int64 ((Seq.length (phrases)) - 1)) do
                    res <- (res + ", ") + (_idx phrases (int i))
                    i <- i + (int64 1)
                res <- (res + ", and ") + (_idx phrases (int ((Seq.length (phrases)) - 1)))
        __ret <- ((subA + " ") + res) + "."
        raise Return
        __ret
    with
        | Return -> __ret
and answerLine (line: string) (facts: obj array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable line = line
    let mutable facts = facts
    try
        if (startsWith (line) ("do ")) || (startsWith (line) ("does ")) then
            let core: string = _substring line 0 ((String.length (line)) - 1)
            let mutable parts: string array = splitSpaces (core)
            let sub: string = _idx parts (int 1)
            let pred: string = _idx parts (int 2)
            let mutable obj: string = if (Seq.length (parts)) > 3 then (join (Array.sub parts 3 ((Seq.length (parts)) - 3)) (" ")) else ""
            __ret <- answerDo (sub) (pred) (obj) (facts)
            raise Return
        if startsWith (line) ("who ") then
            let core: string = _substring line 0 ((String.length (line)) - 1)
            let mutable parts: string array = splitSpaces (core)
            let predWord: string = _idx parts (int 1)
            let pred: string = _substring predWord 0 ((String.length (predWord)) - 1)
            let mutable obj: string = if (Seq.length (parts)) > 2 then (join (Array.sub parts 2 ((Seq.length (parts)) - 2)) (" ")) else ""
            __ret <- answerWho (pred) (obj) (facts)
            raise Return
        let core: string = _substring line 0 ((String.length (line)) - 1)
        let mutable parts: string array = splitSpaces (core)
        let sub: string = _idx parts (int 2)
        __ret <- answerWhat (sub) (facts)
        raise Return
        __ret
    with
        | Return -> __ret
and processDialogue (num: int64) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable num = num
    try
        ignore (printfn "%s" (("Dialogue #" + (_str (num))) + ":"))
        let mutable facts: obj array = [||]
        let mutable contr: bool = false
        try
            while true do
                try
                    let line: string = _readLine()
                    if line = "" then
                        raise Break
                    if endsWith (line) ("!") then
                        ignore (printfn "%s" (line))
                        ignore (printfn "%s" (""))
                        raise Break
                    else
                        if endsWith (line) ("?") then
                            ignore (printfn "%s" (line))
                            let ans: string = if contr then "I am abroad." else (answerLine (line) (facts))
                            ignore (printfn "%s" (ans))
                            ignore (printfn "%s" (""))
                        else
                            let f: obj = parseStatement (line)
                            if (not contr) && (causesContradiction (facts) (f)) then
                                contr <- true
                            facts <- Array.append facts [|unbox<obj> (f)|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let tStr: string = _readLine()
        if tStr = "" then
            __ret <- ()
            raise Return
        let t: int64 = parseIntStr (tStr)
        let mutable i: int64 = int64 1
        while i <= t do
            ignore (processDialogue (int64 i))
            i <- i + (int64 1)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
