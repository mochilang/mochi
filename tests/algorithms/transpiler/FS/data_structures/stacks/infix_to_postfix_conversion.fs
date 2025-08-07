// Generated 2025-08-07 14:57 +0700

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

let PRECEDENCES: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("+", 1); ("-", 1); ("*", 2); ("/", 2); ("^", 3)]
let ASSOCIATIVITIES: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("+", "LR"); ("-", "LR"); ("*", "LR"); ("/", "LR"); ("^", "RL")]
let rec precedence (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        __ret <- if PRECEDENCES.ContainsKey(ch) then (PRECEDENCES.[(string (ch))]) else (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and associativity (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    try
        __ret <- if ASSOCIATIVITIES.ContainsKey(ch) then (ASSOCIATIVITIES.[(string (ch))]) else ""
        raise Return
        __ret
    with
        | Return -> __ret
and balanced_parentheses (expr: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable expr = expr
    try
        let mutable count: int = 0
        let mutable i: int = 0
        while i < (String.length (expr)) do
            let ch: string = _substring expr i (i + 1)
            if ch = "(" then
                count <- count + 1
            if ch = ")" then
                count <- count - 1
                if count < 0 then
                    __ret <- false
                    raise Return
            i <- i + 1
        __ret <- count = 0
        raise Return
        __ret
    with
        | Return -> __ret
and is_letter (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (("a" <= ch) && (ch <= "z")) || (("A" <= ch) && (ch <= "Z"))
        raise Return
        __ret
    with
        | Return -> __ret
and is_digit (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- ("0" <= ch) && (ch <= "9")
        raise Return
        __ret
    with
        | Return -> __ret
and is_alnum (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        __ret <- (is_letter (ch)) || (is_digit (ch))
        raise Return
        __ret
    with
        | Return -> __ret
and infix_to_postfix (expression: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable expression = expression
    try
        if (balanced_parentheses (expression)) = false then
            failwith ("Mismatched parentheses")
        let mutable stack: string array = [||]
        let mutable postfix: string array = [||]
        let mutable i: int = 0
        try
            while i < (String.length (expression)) do
                try
                    let ch: string = _substring expression i (i + 1)
                    if is_alnum (ch) then
                        postfix <- Array.append postfix [|ch|]
                    else
                        if ch = "(" then
                            stack <- Array.append stack [|ch|]
                        else
                            if ch = ")" then
                                while ((Seq.length (stack)) > 0) && ((_idx stack ((Seq.length (stack)) - 1)) <> "(") do
                                    postfix <- Array.append postfix [|_idx stack ((Seq.length (stack)) - 1)|]
                                    stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                                if (Seq.length (stack)) > 0 then
                                    stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                            else
                                if ch = " " then ()
                                else
                                    try
                                        while true do
                                            try
                                                if (Seq.length (stack)) = 0 then
                                                    stack <- Array.append stack [|ch|]
                                                    raise Break
                                                let cp: int = precedence (ch)
                                                let tp: int = precedence (_idx stack ((Seq.length (stack)) - 1))
                                                if cp > tp then
                                                    stack <- Array.append stack [|ch|]
                                                    raise Break
                                                if cp < tp then
                                                    postfix <- Array.append postfix [|_idx stack ((Seq.length (stack)) - 1)|]
                                                    stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                                                    raise Continue
                                                if (associativity (ch)) = "RL" then
                                                    stack <- Array.append stack [|ch|]
                                                    raise Break
                                                postfix <- Array.append postfix [|_idx stack ((Seq.length (stack)) - 1)|]
                                                stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                                            with
                                            | Continue -> ()
                                            | Break -> raise Break
                                    with
                                    | Break -> ()
                                    | Continue -> ()
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        while (Seq.length (stack)) > 0 do
            postfix <- Array.append postfix [|_idx stack ((Seq.length (stack)) - 1)|]
            stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
        let mutable res: string = ""
        let mutable j: int = 0
        while j < (Seq.length (postfix)) do
            if j > 0 then
                res <- res + " "
            res <- res + (_idx postfix (j))
            j <- j + 1
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
        let expression: string = "a+b*(c^d-e)^(f+g*h)-i"
        printfn "%s" (expression)
        printfn "%s" (infix_to_postfix (expression))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
