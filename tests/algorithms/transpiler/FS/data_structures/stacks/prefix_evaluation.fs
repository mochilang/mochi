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
let rec split_custom (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable res: string array = [||]
        let mutable current: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            if ch = sep then
                res <- Array.append res [|current|]
                current <- ""
            else
                current <- current + ch
            i <- i + 1
        res <- Array.append res [|current|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec tokenize (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let parts: string array = split_custom (s) (" ")
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (parts)) do
            let p: string = _idx parts (i)
            if p <> "" then
                res <- Array.append res [|p|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_digit (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (ch >= "0") && (ch <= "9")
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_operand (token: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable token = token
    try
        if token = "" then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (String.length (token)) do
            let ch: string = _substring token i (i + 1)
            if not (is_digit (ch)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_int (token: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable token = token
    try
        let mutable res: int = 0
        let mutable i: int = 0
        while i < (String.length (token)) do
            res <- (res * 10) + (int (_substring token i (i + 1)))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec apply_op (op: string) (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable op = op
    let mutable a = a
    let mutable b = b
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
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec evaluate (expression: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable expression = expression
    try
        let tokens: string array = tokenize (expression)
        let mutable stack: float array = [||]
        let mutable i: int = (Seq.length (tokens)) - 1
        while i >= 0 do
            let token: string = _idx tokens (i)
            if token <> "" then
                if is_operand (token) then
                    stack <- Array.append stack [|float (to_int (token))|]
                else
                    let o1: float = _idx stack ((Seq.length (stack)) - 1)
                    let o2: float = _idx stack ((Seq.length (stack)) - 2)
                    stack <- Array.sub stack 0 (((Seq.length (stack)) - 2) - 0)
                    let mutable res: float = apply_op (token) (o1) (o2)
                    stack <- Array.append stack [|res|]
            i <- i - 1
        __ret <- _idx stack (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec eval_rec (tokens: string array) (pos: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable tokens = tokens
    let mutable pos = pos
    try
        let token: string = _idx tokens (pos)
        let next: int = pos + 1
        if is_operand (token) then
            __ret <- unbox<float array> [|float (to_int (token)); float next|]
            raise Return
        let left: float array = eval_rec (tokens) (next)
        let a: float = _idx left (0)
        let p1: int = unbox<int> (_idx left (1))
        let right: float array = eval_rec (tokens) (p1)
        let b: float = _idx right (0)
        let p2: float = _idx right (1)
        __ret <- unbox<float array> [|apply_op (token) (a) (b); p2|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec evaluate_recursive (expression: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable expression = expression
    try
        let tokens: string array = tokenize (expression)
        let mutable res: float array = eval_rec (tokens) (0)
        __ret <- _idx res (0)
        raise Return
        __ret
    with
        | Return -> __ret
let test_expression: string = "+ 9 * 2 6"
printfn "%s" (_str (evaluate (test_expression)))
let test_expression2: string = "/ * 10 2 + 4 1 "
printfn "%s" (_str (evaluate (test_expression2)))
let test_expression3: string = "+ * 2 3 / 8 4"
printfn "%s" (_str (evaluate_recursive (test_expression3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
