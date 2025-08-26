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
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
open System.Collections.Generic

open System

let digits: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
let digitStr: System.Collections.Generic.IDictionary<int, string> = _dictCreate [(0, "0"); (1, "1"); (2, "2"); (3, "3"); (4, "4"); (5, "5"); (6, "6"); (7, "7"); (8, "8"); (9, "9")]
let rec intToStr (n: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = (int64 0) then
            __ret <- "0"
            raise Return
        let mutable s: string = ""
        let mutable x: int64 = n
        let mutable parts = [||]
        while x > (int64 0) do
            parts <- Array.append parts [|(((x % (int64 10) + (int64 10)) % (int64 10)))|]
            x <- _floordiv64 (int64 x) (int64 (int64 10))
        let mutable i: int64 = int64 ((Seq.length (parts)) - 1)
        while i >= (int64 0) do
            let d: int64 = int64 (_idx parts (int i))
            s <- s + (_dictGet digitStr ((int (d))))
            i <- i - (int64 1)
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and parseIntStr (str: string) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable str = str
    try
        let mutable i: int64 = int64 0
        let mutable n: int64 = int64 0
        while i < (int64 (String.length (str))) do
            n <- (n * (int64 10)) + (int64 (_dictGet digits ((string (string (str.[int i]))))))
            i <- i + (int64 1)
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and segmentCost (p: int64 array) (w: int64 array) (i: int64) (j: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable p = p
    let mutable w = w
    let mutable i = i
    let mutable j = j
    try
        __ret <- ((_idx w (int j)) - (_idx w (int (i - (int64 1))))) - ((i - (int64 1)) * ((_idx p (int j)) - (_idx p (int (i - (int64 1))))))
        raise Return
        __ret
    with
        | Return -> __ret
and better (cur: int64 array) (cand: int64 array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable cur = cur
    let mutable cand = cand
    try
        let mutable i: int64 = int64 ((Seq.length (cur)) - 1)
        let mutable j: int64 = int64 ((Seq.length (cand)) - 1)
        while (i >= (int64 0)) && (j >= (int64 0)) do
            let ci: int64 = int64 (_idx cur (int i))
            let cj: int64 = int64 (_idx cand (int j))
            if cj > ci then
                __ret <- true
                raise Return
            if cj < ci then
                __ret <- false
                raise Return
            i <- i - (int64 1)
            j <- j - (int64 1)
        __ret <- false
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
        let T: int64 = parseIntStr (tStr)
        let mutable case: int64 = int64 1
        try
            for _ in 0 .. ((int T) - 1) do
                try
                    let line: string = _readLine()
                    let mutable pos: int64 = int64 0
                    let mutable K: int64 = int64 0
                    while (pos < (int64 (String.length (line)))) && ((string (line.[int pos])) <> " ") do
                        K <- (K * (int64 10)) + (int64 (_dictGet digits ((string (string (line.[int pos]))))))
                        pos <- pos + (int64 1)
                    pos <- pos + (int64 1)
                    let mutable L: int64 = int64 0
                    while pos < (int64 (String.length (line))) do
                        L <- (L * (int64 10)) + (int64 (_dictGet digits ((string (string (line.[int pos]))))))
                        pos <- pos + (int64 1)
                    let keys: string = _readLine()
                    let letters: string = _readLine()
                    let mutable freq = [||]
                    for _ in 0 .. ((int L) - 1) do
                        freq <- Array.append freq [|(parseIntStr (_readLine()))|]
                    let mutable p: int array = unbox<int array> [|0|]
                    let mutable w: int array = unbox<int array> [|0|]
                    let mutable i: int64 = int64 0
                    while i < L do
                        let fi: int64 = int64 (_idx freq (int i))
                        p <- Array.append p [|int ((int64 (_idx p (int i))) + fi)|]
                        w <- Array.append w [|int ((int64 (_idx w (int i))) + (fi * (i + (int64 1))))|]
                        i <- i + (int64 1)
                    let INF: int64 = 1000000000000000000L
                    let mutable cost = [||]
                    let mutable sizes = [||]
                    i <- int64 0
                    while i <= L do
                        let mutable rowC = [||]
                        let mutable rowS = [||]
                        let mutable k: int64 = int64 0
                        while k <= K do
                            rowC <- Array.append rowC [|INF|]
                            rowS <- Array.append rowS [|Array.empty<int64>|]
                            k <- k + (int64 1)
                        cost <- Array.append cost [|rowC|]
                        sizes <- Array.append sizes [|rowS|]
                        i <- i + (int64 1)
                    cost.[0].[0] <- int64 0
                    sizes.[0].[0] <- [||]
                    i <- int64 1
                    try
                        while i <= L do
                            try
                                let mutable k: int64 = int64 1
                                try
                                    while (k <= K) && (k <= i) do
                                        try
                                            let mutable t: int64 = k - (int64 1)
                                            try
                                                while t < i do
                                                    try
                                                        let prev: int64 = int64 (_idx (_idx cost (int t)) (int (k - (int64 1))))
                                                        if prev = INF then
                                                            t <- t + (int64 1)
                                                            raise Continue
                                                        let c: int64 = prev + (segmentCost (Array.map int64 p) (Array.map int64 w) (int64 (t + (int64 1))) (int64 i))
                                                        let candSizes: int64 array = Array.append (unbox<int64 array> (_idx (_idx sizes (int t)) (int (k - (int64 1))))) [|(i - t)|]
                                                        let curC: int64 = int64 (_idx (_idx cost (int i)) (int k))
                                                        if (c < curC) || ((c = curC) && (better (unbox<int64 array> (_idx (_idx sizes (int i)) (int k))) (candSizes))) then
                                                            cost.[int i].[int k] <- c
                                                            sizes.[int i].[int k] <- unbox<int64 array> candSizes
                                                        t <- t + (int64 1)
                                                    with
                                                    | Continue -> ()
                                                    | Break -> raise Break
                                            with
                                            | Break -> ()
                                            | Continue -> ()
                                            k <- k + (int64 1)
                                        with
                                        | Continue -> ()
                                        | Break -> raise Break
                                with
                                | Break -> ()
                                | Continue -> ()
                                i <- i + (int64 1)
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    let gs: int64 array = unbox<int64 array> (_idx (_idx sizes (int L)) (int K))
                    ignore (printfn "%s" (("Keypad #" + (intToStr (int64 case))) + ":"))
                    let mutable idx: int64 = int64 0
                    let mutable keyIdx: int64 = int64 0
                    while keyIdx < K do
                        let keyCh: string = string (keys.[int keyIdx])
                        let sz: int64 = int64 (_idx gs (int keyIdx))
                        let mutable lineOut: string = keyCh + ": "
                        let mutable j: int64 = int64 0
                        while j < sz do
                            lineOut <- lineOut + (string (letters.[int idx]))
                            idx <- idx + (int64 1)
                            j <- j + (int64 1)
                        ignore (printfn "%s" (lineOut))
                        keyIdx <- keyIdx + (int64 1)
                    ignore (printfn "%s" (""))
                    case <- case + (int64 1)
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
