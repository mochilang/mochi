// Generated 2025-08-07 14:57 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec slice_without_last (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec parse_float (token: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable token = token
    try
        let mutable sign: float = 1.0
        let mutable idx: int = 0
        if (String.length (token)) > 0 then
            let first: string = _substring token 0 1
            if first = "-" then
                sign <- -1.0
                idx <- 1
            else
                if first = "+" then
                    idx <- 1
        let mutable int_part: int = 0
        while (idx < (String.length (token))) && ((_substring token idx (idx + 1)) <> ".") do
            int_part <- (int_part * 10) + (int (int (_substring token idx (idx + 1))))
            idx <- idx + 1
        let mutable result: float = 1.0 * (float int_part)
        if (idx < (String.length (token))) && ((_substring token idx (idx + 1)) = ".") then
            idx <- idx + 1
            let mutable place: float = 0.1
            while idx < (String.length (token)) do
                let digit: int = int (_substring token idx (idx + 1))
                result <- result + (place * (1.0 * (float digit)))
                place <- place / 10.0
                idx <- idx + 1
        __ret <- sign * result
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow_float (``base``: float) (exp: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        let e: int = int (exp)
        while i < e do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec apply_op (a: float) (b: float) (op: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    let mutable op = op
    try
        if op = "+" then
            __ret <- a + b
            raise Return
        if op = "-" then
            __ret <- a - b
            raise Return
        if op = "*" then
            __ret <- a * b
            raise Return
        if op = "/" then
            __ret <- a / b
            raise Return
        if op = "^" then
            __ret <- pow_float (a) (b)
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec evaluate (tokens: string array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable tokens = tokens
    try
        if (Seq.length (tokens)) = 0 then
            __ret <- 0.0
            raise Return
        let mutable stack: float array = [||]
        for token in Seq.map string (tokens) do
            if ((((token = "+") || (token = "-")) || (token = "*")) || (token = "/")) || (token = "^") then
                if ((token = "+") || (token = "-")) && ((Seq.length (stack)) < 2) then
                    let b: float = _idx stack ((Seq.length (stack)) - 1)
                    stack <- slice_without_last (stack)
                    if token = "-" then
                        stack <- Array.append stack [|0.0 - b|]
                    else
                        stack <- Array.append stack [|b|]
                else
                    let b: float = _idx stack ((Seq.length (stack)) - 1)
                    stack <- slice_without_last (stack)
                    let a: float = _idx stack ((Seq.length (stack)) - 1)
                    stack <- slice_without_last (stack)
                    let mutable result: float = apply_op (a) (b) (token)
                    stack <- Array.append stack [|result|]
            else
                stack <- Array.append stack [|parse_float (token)|]
        if (Seq.length (stack)) <> 1 then
            failwith ("Invalid postfix expression")
        __ret <- _idx stack (0)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (evaluate (unbox<string array> [|"2"; "1"; "+"; "3"; "*"|])))
printfn "%s" (_str (evaluate (unbox<string array> [|"4"; "13"; "5"; "/"; "+"|])))
printfn "%s" (_str (evaluate (unbox<string array> [|"5"; "6"; "9"; "*"; "+"|])))
printfn "%s" (_str (evaluate (unbox<string array> [|"2"; "-"; "3"; "+"|])))
printfn "%s" (_str (evaluate (Array.empty<string>)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
