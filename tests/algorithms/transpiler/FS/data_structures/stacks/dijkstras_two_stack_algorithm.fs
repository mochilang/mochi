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
let rec is_digit (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (((((((((ch = "0") || (ch = "1")) || (ch = "2")) || (ch = "3")) || (ch = "4")) || (ch = "5")) || (ch = "6")) || (ch = "7")) || (ch = "8")) || (ch = "9")
        raise Return
        __ret
    with
        | Return -> __ret
let rec slice_without_last_int (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec slice_without_last_string (xs: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    try
        let mutable res: string array = [||]
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec dijkstras_two_stack_algorithm (equation: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable equation = equation
    try
        let mutable operand_stack: int array = [||]
        let mutable operator_stack: string array = [||]
        let mutable idx: int = 0
        while idx < (String.length (equation)) do
            let ch: string = _substring equation idx (idx + 1)
            if is_digit (ch) then
                operand_stack <- Array.append operand_stack [|unbox<int> (int (ch))|]
            else
                if (((ch = "+") || (ch = "-")) || (ch = "*")) || (ch = "/") then
                    operator_stack <- Array.append operator_stack [|ch|]
                else
                    if ch = ")" then
                        let opr: string = _idx operator_stack ((Seq.length (operator_stack)) - 1)
                        operator_stack <- slice_without_last_string (operator_stack)
                        let num1: int = _idx operand_stack ((Seq.length (operand_stack)) - 1)
                        operand_stack <- slice_without_last_int (operand_stack)
                        let num2: int = _idx operand_stack ((Seq.length (operand_stack)) - 1)
                        operand_stack <- slice_without_last_int (operand_stack)
                        let total: int = if opr = "+" then (num2 + num1) else (if opr = "-" then (num2 - num1) else (if opr = "*" then (num2 * num1) else (num2 / num1)))
                        operand_stack <- Array.append operand_stack [|total|]
            idx <- idx + 1
        __ret <- _idx operand_stack ((Seq.length (operand_stack)) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
let equation: string = "(5 + ((4 * 2) * (2 + 3)))"
printfn "%s" ((equation + " = ") + (_str (dijkstras_two_stack_algorithm (equation))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
