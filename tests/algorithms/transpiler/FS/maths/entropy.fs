// Generated 2025-08-12 07:47 +0700

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

let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type TextCounts = {
    mutable _single: System.Collections.Generic.IDictionary<string, int>
    mutable _double: System.Collections.Generic.IDictionary<string, int>
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec log2 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable k: float = 0.0
        let mutable v: float = x
        while v >= 2.0 do
            v <- v / 2.0
            k <- k + 1.0
        while v < 1.0 do
            v <- v * 2.0
            k <- k - 1.0
        let z: float = (v - 1.0) / (v + 1.0)
        let mutable zpow: float = z
        let mutable sum: float = z
        let mutable i: int = 3
        while i <= 9 do
            zpow <- (zpow * z) * z
            sum <- sum + (zpow / (float i))
            i <- i + 2
        let ln2: float = 0.6931471805599453
        __ret <- k + ((2.0 * sum) / ln2)
        raise Return
        __ret
    with
        | Return -> __ret
and analyze_text (text: string) =
    let mutable __ret : TextCounts = Unchecked.defaultof<TextCounts>
    let mutable text = text
    try
        let mutable _single: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable _double: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let n: int = String.length (text)
        if n = 0 then
            __ret <- { _single = _single; _double = _double }
            raise Return
        let last: string = _substring text (n - 1) n
        if _single.ContainsKey(last) then
            _single.[last] <- (_dictGet _single ((string (last)))) + 1
        else
            _single.[last] <- 1
        let first: string = _substring text 0 1
        let pair0: string = " " + first
        _double.[pair0] <- 1
        let mutable i: int = 0
        while i < (n - 1) do
            let ch: string = _substring text i (i + 1)
            if _single.ContainsKey(ch) then
                _single.[ch] <- (_dictGet _single ((string (ch)))) + 1
            else
                _single.[ch] <- 1
            let seq: string = _substring text i (i + 2)
            if _double.ContainsKey(seq) then
                _double.[seq] <- (_dictGet _double ((string (seq)))) + 1
            else
                _double.[seq] <- 1
            i <- i + 1
        __ret <- { _single = _single; _double = _double }
        raise Return
        __ret
    with
        | Return -> __ret
and round_to_int (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (int (x - 0.5)) else (int (x + 0.5))
        raise Return
        __ret
    with
        | Return -> __ret
and calculate_entropy (text: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable text = text
    try
        let counts: TextCounts = analyze_text (text)
        let alphas: string = " abcdefghijklmnopqrstuvwxyz"
        let mutable total1: int = 0
        for ch in (counts._single).Keys do
            total1 <- total1 + (_dictGet (counts._single) ((string (ch))))
        let mutable h1: float = 0.0
        let mutable i: int = 0
        while i < (String.length (alphas)) do
            let ch: string = _substring alphas i (i + 1)
            if (counts._single).ContainsKey(ch) then
                let prob: float = (float (_dictGet (counts._single) ((string (ch))))) / (float total1)
                h1 <- h1 + (prob * (log2 (prob)))
            i <- i + 1
        let first_entropy: float = -h1
        printfn "%s" ((_str (round_to_int (first_entropy))) + ".0")
        let mutable total2: int = 0
        for seq in (counts._double).Keys do
            total2 <- total2 + (_dictGet (counts._double) ((string (seq))))
        let mutable h2: float = 0.0
        let mutable a0: int = 0
        while a0 < (String.length (alphas)) do
            let ch0: string = _substring alphas a0 (a0 + 1)
            let mutable a1: int = 0
            while a1 < (String.length (alphas)) do
                let ch1: string = _substring alphas a1 (a1 + 1)
                let seq: string = ch0 + ch1
                if (counts._double).ContainsKey(seq) then
                    let prob: float = (float (_dictGet (counts._double) ((string (seq))))) / (float total2)
                    h2 <- h2 + (prob * (log2 (prob)))
                a1 <- a1 + 1
            a0 <- a0 + 1
        let second_entropy: float = -h2
        printfn "%s" ((_str (round_to_int (second_entropy))) + ".0")
        let diff: float = second_entropy - first_entropy
        printfn "%s" ((_str (round_to_int (diff))) + ".0")
        __ret
    with
        | Return -> __ret
let text1: string = ("Behind Winston's back the voice " + "from the telescreen was still ") + "babbling and the overfulfilment"
calculate_entropy (text1)
let text3: string = ((((((((("Had repulsive dashwoods suspicion sincerity but advantage now him. " + "Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. ") + "You greatest jointure saw horrible. He private he on be imagine ") + "suppose. Fertile beloved evident through no service elderly is. Blind ") + "there if every no so at. Own neglected you preferred way sincerity ") + "delivered his attempted. To of message cottage windows do besides ") + "against uncivil.  Delightful unreserved impossible few estimating ") + "men favourable see entreaties. She propriety immediate was improving. ") + "He or entrance humoured likewise moderate. Much nor game son say ") + "feel. Fat make met can must form into gate. Me we offending prevailed ") + "discovery."
calculate_entropy (text3)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
