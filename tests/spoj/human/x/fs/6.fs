// Generated 2025-08-27 07:05 +0700

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
let toi (v:obj) : int =
    match v with
    | :? int as n -> n
    | :? int64 as n -> int n
    | :? float as f -> int f
    | :? string as s ->
        match System.Int32.TryParse(s) with
        | true, n -> n
        | _ -> 0
    | _ -> 0

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
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
open System.Collections.Generic

open System

let digitMap: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
let rec repeat (s: string) (n: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable n = n
    try
        let mutable r: string = ""
        for _ in 0 .. ((int n) - 1) do
            r <- r + s
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and add_str (a: string) (b: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    let mutable b = b
    try
        let mutable i: int64 = int64 ((String.length (a)) - 1)
        let mutable j: int64 = int64 ((String.length (b)) - 1)
        let mutable carry: int64 = int64 0
        let mutable res: string = ""
        while ((i >= (int64 0)) || (j >= (int64 0))) || (carry > (int64 0)) do
            let mutable da: int64 = int64 0
            if i >= (int64 0) then
                da <- int64 (_dictGet digitMap ((string (_substring a (int i) (int (i + (int64 1)))))))
            let mutable db: int64 = int64 0
            if j >= (int64 0) then
                db <- int64 (_dictGet digitMap ((string (_substring b (int j) (int (j + (int64 1)))))))
            let mutable sum: int64 = (da + db) + carry
            res <- (_str (((sum % (int64 10) + (int64 10)) % (int64 10)))) + res
            carry <- _floordiv64 (int64 sum) (int64 (int64 10))
            i <- i - (int64 1)
            j <- j - (int64 1)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and sub_str (a: string) (b: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    let mutable b = b
    try
        let mutable i: int64 = int64 ((String.length (a)) - 1)
        let mutable j: int64 = int64 ((String.length (b)) - 1)
        let mutable borrow: int64 = int64 0
        let mutable res: string = ""
        while i >= (int64 0) do
            let mutable da: int64 = (int64 (_dictGet digitMap ((string (_substring a (int i) (int (i + (int64 1)))))))) - borrow
            let mutable db: int64 = int64 0
            if j >= (int64 0) then
                db <- int64 (_dictGet digitMap ((string (_substring b (int j) (int (j + (int64 1)))))))
            if da < db then
                da <- da + (int64 10)
                borrow <- int64 1
            else
                borrow <- int64 0
            let mutable diff: int64 = da - db
            res <- (_str (diff)) + res
            i <- i - (int64 1)
            j <- j - (int64 1)
        let mutable k: int64 = int64 0
        while (k < (int64 (String.length (res)))) && ((_substring res (int k) (int (k + (int64 1)))) = "0") do
            k <- k + (int64 1)
        if k = (int64 (String.length (res))) then
            __ret <- "0"
            raise Return
        __ret <- _substring res (int k) (String.length (res))
        raise Return
        __ret
    with
        | Return -> __ret
and mul_digit (a: string) (d: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    let mutable d = d
    try
        if d = (int64 0) then
            __ret <- "0"
            raise Return
        let mutable i: int64 = int64 ((String.length (a)) - 1)
        let mutable carry: int64 = int64 0
        let mutable res: string = ""
        while i >= (int64 0) do
            let mutable prod: int64 = ((int64 (_dictGet digitMap ((string (_substring a (int i) (int (i + (int64 1)))))))) * d) + carry
            res <- (_str (((prod % (int64 10) + (int64 10)) % (int64 10)))) + res
            carry <- _floordiv64 (int64 prod) (int64 (int64 10))
            i <- i - (int64 1)
        if carry > (int64 0) then
            res <- (_str (carry)) + res
        let mutable k: int64 = int64 0
        while (k < (int64 (String.length (res)))) && ((_substring res (int k) (int (k + (int64 1)))) = "0") do
            k <- k + (int64 1)
        if k = (int64 (String.length (res))) then
            __ret <- "0"
            raise Return
        __ret <- _substring res (int k) (String.length (res))
        raise Return
        __ret
    with
        | Return -> __ret
and mul_str (a: string) (b: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, obj> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, obj>>
    let mutable a = a
    let mutable b = b
    try
        let mutable result: string = "0"
        let mutable shift: int64 = int64 0
        let mutable parts: obj array = [||]
        let mutable i: int64 = int64 ((String.length (b)) - 1)
        while i >= (int64 0) do
            let d: int64 = int64 (_dictGet digitMap ((string (_substring b (int i) (int (i + (int64 1)))))))
            let part: string = mul_digit (a) (int64 d)
            parts <- Array.append parts [|box (_dictCreate [("val", box (part)); ("shift", box (shift))])|]
            let mutable shifted: string = part
            for _ in 0 .. ((int shift) - 1) do
                shifted <- shifted + "0"
            result <- add_str (result) (shifted)
            shift <- shift + (int64 1)
            i <- i - (int64 1)
        __ret <- _dictCreate<string, obj> [("res", box (result)); ("parts", box (parts))]
        raise Return
        __ret
    with
        | Return -> __ret
and pad_left (s: string) (total: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable total = total
    try
        let mutable r: string = ""
        for _ in 0 .. ((int (total - (int64 (String.length (s))))) - 1) do
            r <- r + " "
        __ret <- r + s
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
        let t: int64 = int64 (tStr)
        try
            for _ in 0 .. ((int t) - 1) do
                try
                    let mutable line: string = _readLine()
                    if line = "" then
                        raise Continue
                    let mutable idx: int64 = int64 0
                    try
                        while idx < (int64 (String.length (line))) do
                            try
                                let ch: string = _substring line (int idx) (int (idx + (int64 1)))
                                if ((ch = "+") || (ch = "-")) || (ch = "*") then
                                    raise Break
                                idx <- idx + (int64 1)
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    let a: string = _substring line (0) (int idx)
                    let op: string = _substring line (int idx) (int (idx + (int64 1)))
                    let b: string = _substring line (int (idx + (int64 1))) (String.length (line))
                    let mutable res: string = ""
                    let mutable parts: obj array = [||]
                    if op = "+" then
                        res <- add_str (a) (b)
                    else
                        if op = "-" then
                            res <- sub_str (a) (b)
                        else
                            let mutable r: System.Collections.Generic.IDictionary<string, obj> = mul_str (a) (b)
                            res <- unbox<string> (_dictGet r ((string ("res"))))
                            parts <- unbox<obj array> (_dictGet r ((string ("parts"))))
                    let mutable width: int64 = int64 (String.length (a))
                    let secondLen: int64 = int64 ((String.length (b)) + 1)
                    if secondLen > width then
                        width <- secondLen
                    if (int64 (String.length (res))) > width then
                        width <- int64 (String.length (res))
                    for p in parts do
                        let l: int64 = (int64 (String.length (unbox<string> (((p :?> System.Collections.Generic.IDictionary<string, obj>).["val"]))))) + (int64 (toi (((p :?> System.Collections.Generic.IDictionary<string, obj>).["shift"]))))
                        if l > width then
                            width <- l
                    ignore (printfn "%s" (pad_left (a) (int64 width)))
                    ignore (printfn "%s" (pad_left (op + b) (int64 width)))
                    let mutable dash1: int64 = int64 0
                    if op = "*" then
                        if (Seq.length (parts)) > 0 then
                            dash1 <- int64 ((String.length (b)) + 1)
                            let firstPart: string = unbox<string> (((_idx parts (int 0) :?> System.Collections.Generic.IDictionary<string, obj>).["val"]))
                            if (int64 (String.length (firstPart))) > dash1 then
                                dash1 <- int64 (String.length (firstPart))
                        else
                            dash1 <- int64 ((String.length (b)) + 1)
                            if (int64 (String.length (res))) > dash1 then
                                dash1 <- int64 (String.length (res))
                    else
                        dash1 <- int64 ((String.length (b)) + 1)
                        if (int64 (String.length (res))) > dash1 then
                            dash1 <- int64 (String.length (res))
                    ignore (printfn "%s" (pad_left (repeat ("-") (int64 dash1)) (int64 width)))
                    if (op = "*") && ((String.length (b)) > 1) then
                        for p in parts do
                            let ``val``: string = unbox<string> (((p :?> System.Collections.Generic.IDictionary<string, obj>).["val"]))
                            let mutable shift: int64 = int64 (toi (((p :?> System.Collections.Generic.IDictionary<string, obj>).["shift"])))
                            let spaces: int64 = (width - shift) - (int64 (String.length (``val``)))
                            let mutable line: string = ""
                            for _ in 0 .. ((int spaces) - 1) do
                                line <- line + " "
                            line <- line + ``val``
                            ignore (printfn "%s" (line))
                        ignore (printfn "%s" (pad_left (repeat ("-") (int64 (String.length (res)))) (int64 width)))
                    ignore (printfn "%s" (pad_left (res) (int64 width)))
                    ignore (printfn "%s" (""))
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
