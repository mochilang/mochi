// Generated 2025-08-08 16:34 +0700

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
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type DecodeResult = {
    mutable _data: int array
    mutable _ack: bool
}
let rec index_of (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        let mutable idx: int = index_of (upper) (ch)
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- index_of (lower) (ch)
        if idx >= 0 then
            __ret <- 97 + idx
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and chr (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        if (n >= 65) && (n < 91) then
            __ret <- _substring upper (n - 65) (n - 64)
            raise Return
        if (n >= 97) && (n < 123) then
            __ret <- _substring lower (n - 97) (n - 96)
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
and text_to_bits (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    try
        let mutable bits: string = ""
        let mutable i: int = 0
        while i < (String.length (text)) do
            let mutable code: int = ord (string (text.[i]))
            let mutable j: int = 7
            while j >= 0 do
                let mutable p: int = pow2 (j)
                if ((((_floordiv code p) % 2 + 2) % 2)) = 1 then
                    bits <- bits + "1"
                else
                    bits <- bits + "0"
                j <- j - 1
            i <- i + 1
        __ret <- bits
        raise Return
        __ret
    with
        | Return -> __ret
and text_from_bits (bits: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bits = bits
    try
        let mutable text: string = ""
        let mutable i: int = 0
        while i < (String.length (bits)) do
            let mutable code: int = 0
            let mutable j: int = 0
            while (j < 8) && ((i + j) < (String.length (bits))) do
                code <- code * 2
                if (string (bits.[i + j])) = "1" then
                    code <- code + 1
                j <- j + 1
            text <- text + (chr (code))
            i <- i + 8
        __ret <- text
        raise Return
        __ret
    with
        | Return -> __ret
and bool_to_string (b: bool) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        __ret <- if b then "True" else "False"
        raise Return
        __ret
    with
        | Return -> __ret
and string_to_bitlist (s: string) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (string (s.[i])) = "1" then
                res <- Array.append res [|1|]
            else
                res <- Array.append res [|0|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and bitlist_to_string (bits: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable bits = bits
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < (Seq.length (bits)) do
            if (_idx bits (i)) = 1 then
                s <- s + "1"
            else
                s <- s + "0"
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and is_power_of_two (x: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable x = x
    try
        if x < 1 then
            __ret <- false
            raise Return
        let mutable p: int = 1
        while p < x do
            p <- p * 2
        __ret <- p = x
        raise Return
        __ret
    with
        | Return -> __ret
and list_eq (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (i)) <> (_idx b (i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and pow2 (e: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable e = e
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < e do
            res <- res * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and has_bit (n: int) (b: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    let mutable b = b
    try
        let mutable p: int = pow2 (b)
        if ((((_floordiv n p) % 2 + 2) % 2)) = 1 then
            __ret <- true
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and hamming_encode (r: int) (data_bits: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable r = r
    let mutable data_bits = data_bits
    try
        let total: int = r + (Seq.length (data_bits))
        let mutable data_ord: int array = [||]
        let mutable cont_data: int = 0
        let mutable x: int = 1
        while x <= total do
            if is_power_of_two (x) then
                data_ord <- Array.append data_ord [|(-1)|]
            else
                data_ord <- Array.append data_ord [|(_idx data_bits (cont_data))|]
                cont_data <- cont_data + 1
            x <- x + 1
        let mutable parity: int array = [||]
        let mutable bp: int = 0
        while bp < r do
            let mutable cont_bo: int = 0
            let mutable j: int = 0
            while j < (Seq.length (data_ord)) do
                let bit: int = _idx data_ord (j)
                if bit >= 0 then
                    let pos: int = j + 1
                    if (has_bit (pos) (bp)) && (bit = 1) then
                        cont_bo <- cont_bo + 1
                j <- j + 1
            parity <- Array.append parity [|(((cont_bo % 2 + 2) % 2))|]
            bp <- bp + 1
        let mutable result: int array = [||]
        let mutable cont_bp: int = 0
        let mutable i: int = 0
        while i < (Seq.length (data_ord)) do
            if (_idx data_ord (i)) < 0 then
                result <- Array.append result [|(_idx parity (cont_bp))|]
                cont_bp <- cont_bp + 1
            else
                result <- Array.append result [|(_idx data_ord (i))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and hamming_decode (r: int) (code: int array) =
    let mutable __ret : DecodeResult = Unchecked.defaultof<DecodeResult>
    let mutable r = r
    let mutable code = code
    try
        let mutable data_output: int array = [||]
        let mutable parity_received: int array = [||]
        let mutable i: int = 1
        let mutable idx: int = 0
        while i <= (Seq.length (code)) do
            if is_power_of_two (i) then
                parity_received <- Array.append parity_received [|(_idx code (idx))|]
            else
                data_output <- Array.append data_output [|(_idx code (idx))|]
            idx <- idx + 1
            i <- i + 1
        let recomputed: int array = hamming_encode (r) (data_output)
        let mutable parity_calc: int array = [||]
        let mutable j: int = 0
        while j < (Seq.length (recomputed)) do
            if is_power_of_two (j + 1) then
                parity_calc <- Array.append parity_calc [|(_idx recomputed (j))|]
            j <- j + 1
        let _ack: bool = list_eq (parity_received) (parity_calc)
        __ret <- { _data = data_output; _ack = _ack }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let sizePari: int = 4
        let be: int = 2
        let mutable text: string = "Message01"
        let binary: string = text_to_bits (text)
        printfn "%s" (("Text input in binary is '" + binary) + "'")
        let data_bits: int array = string_to_bitlist (binary)
        let encoded: int array = hamming_encode (sizePari) (data_bits)
        printfn "%s" ("Data converted ----------> " + (bitlist_to_string (encoded)))
        let decoded: DecodeResult = hamming_decode (sizePari) (encoded)
        printfn "%s" ((("Data receive ------------> " + (bitlist_to_string (decoded._data))) + " -- Data integrity: ") + (bool_to_string (decoded._ack)))
        let mutable corrupted: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (encoded)) do
            corrupted <- Array.append corrupted [|(_idx encoded (i))|]
            i <- i + 1
        let pos: int = be - 1
        if (_idx corrupted (pos)) = 0 then
            corrupted.[pos] <- 1
        else
            corrupted.[pos] <- 0
        let decoded_err: DecodeResult = hamming_decode (sizePari) (corrupted)
        printfn "%s" ((("Data receive (error) ----> " + (bitlist_to_string (decoded_err._data))) + " -- Data integrity: ") + (bool_to_string (decoded_err._ack)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
