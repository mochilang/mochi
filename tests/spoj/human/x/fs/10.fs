// Generated 2025-08-27 07:05 +0700

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
and precedence (op: string) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable op = op
    try
        if (op = "+") || (op = "-") then
            __ret <- int64 1
            raise Return
        if (op = "*") || (op = "/") then
            __ret <- int64 2
            raise Return
        __ret <- int64 0
        raise Return
        __ret
    with
        | Return -> __ret
and parse (s: string) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable s = s
    try
        let mutable ops: string array = Array.empty<string>
        let mutable vals: obj array = [||]
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (s))) do
            let ch: string = _substring s (int i) (int (i + (int64 1)))
            if (ch >= "a") && (ch <= "z") then
                vals <- Array.append vals [|box (_dictCreate [("kind", "var"); ("val", ch)])|]
            else
                if ch = "(" then
                    ops <- Array.append ops [|ch|]
                else
                    if ch = ")" then
                        while ((Seq.length (ops)) > 0) && ((_idx ops (int ((Seq.length (ops)) - 1))) <> "(") do
                            let op: string = _idx ops (int ((Seq.length (ops)) - 1))
                            ops <- Array.sub ops 0 (((Seq.length (ops)) - 1) - 0)
                            let right: obj = _idx vals (int ((Seq.length (vals)) - 1))
                            vals <- Array.sub vals 0 (((Seq.length (vals)) - 1) - 0)
                            let left: obj = _idx vals (int ((Seq.length (vals)) - 1))
                            vals <- Array.sub vals 0 (((Seq.length (vals)) - 1) - 0)
                            vals <- Array.append vals [|box (_dictCreate [("kind", box ("op")); ("op", box (op)); ("left", left); ("right", right)])|]
                        ops <- Array.sub ops 0 (((Seq.length (ops)) - 1) - 0)
                    else
                        while (((Seq.length (ops)) > 0) && ((_idx ops (int ((Seq.length (ops)) - 1))) <> "(")) && ((precedence (_idx ops (int ((Seq.length (ops)) - 1)))) >= (precedence (ch))) do
                            let op: string = _idx ops (int ((Seq.length (ops)) - 1))
                            ops <- Array.sub ops 0 (((Seq.length (ops)) - 1) - 0)
                            let right: obj = _idx vals (int ((Seq.length (vals)) - 1))
                            vals <- Array.sub vals 0 (((Seq.length (vals)) - 1) - 0)
                            let left: obj = _idx vals (int ((Seq.length (vals)) - 1))
                            vals <- Array.sub vals 0 (((Seq.length (vals)) - 1) - 0)
                            vals <- Array.append vals [|box (_dictCreate [("kind", box ("op")); ("op", box (op)); ("left", left); ("right", right)])|]
                        ops <- Array.append ops [|ch|]
            i <- i + (int64 1)
        while (Seq.length (ops)) > 0 do
            let op: string = _idx ops (int ((Seq.length (ops)) - 1))
            ops <- Array.sub ops 0 (((Seq.length (ops)) - 1) - 0)
            let right: obj = _idx vals (int ((Seq.length (vals)) - 1))
            vals <- Array.sub vals 0 (((Seq.length (vals)) - 1) - 0)
            let left: obj = _idx vals (int ((Seq.length (vals)) - 1))
            vals <- Array.sub vals 0 (((Seq.length (vals)) - 1) - 0)
            vals <- Array.append vals [|box (_dictCreate [("kind", box ("op")); ("op", box (op)); ("left", left); ("right", right)])|]
        __ret <- _idx vals (int ((Seq.length (vals)) - 1))
        raise Return
        __ret
    with
        | Return -> __ret
and needParen (parent: string) (isRight: bool) (child: obj) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable parent = parent
    let mutable isRight = isRight
    let mutable child = child
    try
        if (unbox<string> (((child :?> System.Collections.Generic.IDictionary<string, obj>).["kind"]))) <> "op" then
            __ret <- false
            raise Return
        let p: int64 = precedence (parent)
        let c: int64 = precedence (unbox<string> (((child :?> System.Collections.Generic.IDictionary<string, obj>).["op"])))
        if c < p then
            __ret <- true
            raise Return
        if c > p then
            __ret <- false
            raise Return
        if ((parent = "-") && isRight) && (((unbox<string> (((child :?> System.Collections.Generic.IDictionary<string, obj>).["op"]))) = "+") || ((unbox<string> (((child :?> System.Collections.Generic.IDictionary<string, obj>).["op"]))) = "-")) then
            __ret <- true
            raise Return
        if ((parent = "/") && isRight) && (((unbox<string> (((child :?> System.Collections.Generic.IDictionary<string, obj>).["op"]))) = "*") || ((unbox<string> (((child :?> System.Collections.Generic.IDictionary<string, obj>).["op"]))) = "/")) then
            __ret <- true
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and formatRec (node: obj) (parent: string) (isRight: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable node = node
    let mutable parent = parent
    let mutable isRight = isRight
    try
        if (unbox<string> (((node :?> System.Collections.Generic.IDictionary<string, obj>).["kind"]))) <> "op" then
            __ret <- unbox<string> (((node :?> System.Collections.Generic.IDictionary<string, obj>).["val"]))
            raise Return
        let mutable left: string = formatRec ((((node :?> System.Collections.Generic.IDictionary<string, obj>).["left"]))) (unbox<string> (((node :?> System.Collections.Generic.IDictionary<string, obj>).["op"]))) (false)
        let mutable right: string = formatRec ((((node :?> System.Collections.Generic.IDictionary<string, obj>).["right"]))) (unbox<string> (((node :?> System.Collections.Generic.IDictionary<string, obj>).["op"]))) (true)
        let mutable res: string = (left + (_str (((node :?> System.Collections.Generic.IDictionary<string, obj>).["op"])))) + right
        if (parent <> "") && (needParen (parent) (isRight) (node)) then
            res <- ("(" + res) + ")"
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and makeNice (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let root: obj = parse (s)
        __ret <- formatRec (root) ("") (false)
        raise Return
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
        for _ in 0 .. ((int t) - 1) do
            let line: string = _readLine()
            ignore (printfn "%s" (makeNice (line)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
