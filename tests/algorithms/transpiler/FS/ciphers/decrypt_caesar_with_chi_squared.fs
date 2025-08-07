// Generated 2025-08-07 00:16 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Result = {
    shift: int
    chi: float
    decoded: string
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec default_alphabet () =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    try
        __ret <- unbox<string array> [|"a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"|]
        raise Return
        __ret
    with
        | Return -> __ret
and default_frequencies () =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    try
        __ret <- _dictCreate [("a", 0.08497); ("b", 0.01492); ("c", 0.02202); ("d", 0.04253); ("e", 0.11162); ("f", 0.02228); ("g", 0.02015); ("h", 0.06094); ("i", 0.07546); ("j", 0.00153); ("k", 0.01292); ("l", 0.04025); ("m", 0.02406); ("n", 0.06749); ("o", 0.07507); ("p", 0.01929); ("q", 0.00095); ("r", 0.07587); ("s", 0.06327); ("t", 0.09356); ("u", 0.02758); ("v", 0.00978); ("w", 0.0256); ("x", 0.0015); ("y", 0.01994); ("z", 0.00077)]
        raise Return
        __ret
    with
        | Return -> __ret
and index_of (xs: string array) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and count_char (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable count: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                count <- count + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and decrypt_caesar_with_chi_squared (ciphertext: string) (cipher_alphabet: string array) (frequencies_dict: System.Collections.Generic.IDictionary<string, float>) (case_sensitive: bool) =
    let mutable __ret : Result = Unchecked.defaultof<Result>
    let mutable ciphertext = ciphertext
    let mutable cipher_alphabet = cipher_alphabet
    let mutable frequencies_dict = frequencies_dict
    let mutable case_sensitive = case_sensitive
    try
        let mutable alphabet_letters: string array = cipher_alphabet
        if (Seq.length (alphabet_letters)) = 0 then
            alphabet_letters <- default_alphabet()
        let mutable frequencies: System.Collections.Generic.IDictionary<string, float> = frequencies_dict
        if (Seq.length (frequencies)) = 0 then
            frequencies <- default_frequencies()
        if not case_sensitive then
            ciphertext <- unbox<string> (ciphertext.ToLower())
        let mutable best_shift: int = 0
        let mutable best_chi: float = 0.0
        let mutable best_text: string = ""
        let mutable shift: int = 0
        while shift < (Seq.length (alphabet_letters)) do
            let mutable decrypted: string = ""
            let mutable i: int = 0
            while i < (String.length (ciphertext)) do
                let ch: string = _substring ciphertext i (i + 1)
                let idx: int = index_of (alphabet_letters) (unbox<string> (ch.ToLower()))
                if idx >= 0 then
                    let m: int = Seq.length (alphabet_letters)
                    let mutable new_idx: int = (((idx - shift) % m + m) % m)
                    if new_idx < 0 then
                        new_idx <- new_idx + m
                    let new_char: string = _idx alphabet_letters (new_idx)
                    if case_sensitive && (ch <> (unbox<string> (ch.ToLower()))) then
                        decrypted <- decrypted + (unbox<string> (new_char.ToUpper()))
                    else
                        decrypted <- decrypted + new_char
                else
                    decrypted <- decrypted + ch
                i <- i + 1
            let mutable chi: float = 0.0
            let lowered = if case_sensitive then (decrypted.ToLower()) else decrypted
            let mutable j: int = 0
            while j < (Seq.length (alphabet_letters)) do
                let letter: string = _idx alphabet_letters (j)
                let occ: int = count_char (unbox<string> lowered) (letter)
                if occ > 0 then
                    let occf: float = float occ
                    let expected: float = (frequencies.[(string (letter))]) * occf
                    let diff: float = occf - expected
                    chi <- chi + (((diff * diff) / expected) * occf)
                j <- j + 1
            if (shift = 0) || (chi < best_chi) then
                best_shift <- shift
                best_chi <- chi
                best_text <- decrypted
            shift <- shift + 1
        __ret <- { shift = best_shift; chi = best_chi; decoded = best_text }
        raise Return
        __ret
    with
        | Return -> __ret
let r1: Result = decrypt_caesar_with_chi_squared ("dof pz aol jhlzhy jpwoly zv wvwbshy? pa pz avv lhzf av jyhjr!") (Array.empty<string>) (_dictCreate []) (false)
printfn "%s" (((((_str (r1.shift)) + ", ") + (_str (r1.chi))) + ", ") + (r1.decoded))
let r2: Result = decrypt_caesar_with_chi_squared ("crybd cdbsxq") (Array.empty<string>) (_dictCreate []) (false)
printfn "%s" (((((_str (r2.shift)) + ", ") + (_str (r2.chi))) + ", ") + (r2.decoded))
let r3: Result = decrypt_caesar_with_chi_squared ("Crybd Cdbsxq") (Array.empty<string>) (_dictCreate []) (true)
printfn "%s" (((((_str (r3.shift)) + ", ") + (_str (r3.chi))) + ", ") + (r3.decoded))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
