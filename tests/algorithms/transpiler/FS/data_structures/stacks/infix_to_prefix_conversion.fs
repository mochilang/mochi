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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let PRIORITY: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("^", 3); ("*", 2); ("/", 2); ("%", 2); ("+", 1); ("-", 1)]
let LETTERS: string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let DIGITS: string = "0123456789"
let rec is_alpha (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (LETTERS)) do
            if (string (LETTERS.[i])) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_digit (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (DIGITS)) do
            if (string (DIGITS.[i])) = ch then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec reverse_string (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable out: string = ""
        let mutable i: int = (String.length (s)) - 1
        while i >= 0 do
            out <- out + (string (s.[i]))
            i <- i - 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec infix_to_postfix (infix: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable infix = infix
    try
        let mutable stack: string array = [||]
        let mutable post: string array = [||]
        let mutable i: int = 0
        while i < (String.length (infix)) do
            let x: string = string (infix.[i])
            if (is_alpha (x)) || (is_digit (x)) then
                post <- Array.append post [|x|]
            else
                if x = "(" then
                    stack <- Array.append stack [|x|]
                else
                    if x = ")" then
                        if (Seq.length (stack)) = 0 then
                            failwith ("list index out of range")
                        while (_idx stack ((Seq.length (stack)) - 1)) <> "(" do
                            post <- Array.append post [|_idx stack ((Seq.length (stack)) - 1)|]
                            stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                        stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                    else
                        if (Seq.length (stack)) = 0 then
                            stack <- Array.append stack [|x|]
                        else
                            while (((Seq.length (stack)) > 0) && ((_idx stack ((Seq.length (stack)) - 1)) <> "(")) && ((PRIORITY.[(string (x))]) <= (PRIORITY.[(string (_idx stack ((Seq.length (stack)) - 1)))])) do
                                post <- Array.append post [|_idx stack ((Seq.length (stack)) - 1)|]
                                stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                            stack <- Array.append stack [|x|]
            i <- i + 1
        while (Seq.length (stack)) > 0 do
            if (_idx stack ((Seq.length (stack)) - 1)) = "(" then
                failwith ("invalid expression")
            post <- Array.append post [|_idx stack ((Seq.length (stack)) - 1)|]
            stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
        let mutable res: string = ""
        let mutable j: int = 0
        while j < (Seq.length (post)) do
            res <- res + (_idx post (j))
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec infix_to_prefix (infix: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable infix = infix
    try
        let mutable reversed: string = ""
        let mutable i: int = (String.length (infix)) - 1
        while i >= 0 do
            let ch: string = string (infix.[i])
            if ch = "(" then
                reversed <- reversed + ")"
            else
                if ch = ")" then
                    reversed <- reversed + "("
                else
                    reversed <- reversed + ch
            i <- i - 1
        let postfix: string = infix_to_postfix (reversed)
        let prefix: string = reverse_string (postfix)
        __ret <- prefix
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
