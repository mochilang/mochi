// Generated 2025-08-07 10:31 +0700

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
type Cipher = {
    passcode: string array
    key_list: string array
    shift_key: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System

let rec ord (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ch = ch
    try
        let digits: string = "0123456789"
        let mutable i: int = 0
        while i < (String.length (digits)) do
            if (_substring digits i (i + 1)) = ch then
                __ret <- 48 + i
                raise Return
            i <- i + 1
        let upper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        i <- 0
        while i < (String.length (upper)) do
            if (_substring upper i (i + 1)) = ch then
                __ret <- 65 + i
                raise Return
            i <- i + 1
        let lower: string = "abcdefghijklmnopqrstuvwxyz"
        i <- 0
        while i < (String.length (lower)) do
            if (_substring lower i (i + 1)) = ch then
                __ret <- 97 + i
                raise Return
            i <- i + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec neg_pos (iterlist: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable iterlist = iterlist
    try
        let mutable i: int = 1
        while i < (Seq.length (iterlist)) do
            iterlist.[i] <- -(_idx iterlist (i))
            i <- i + 2
        __ret <- iterlist
        raise Return
        __ret
    with
        | Return -> __ret
let rec passcode_creator () =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    try
        let choices: string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        let mutable seed: int = _now()
        let length: int = 10 + (((seed % 11 + 11) % 11))
        let mutable password: string array = [||]
        let mutable i: int = 0
        while i < length do
            seed <- ((((seed * 1103515245) + 12345) % 2147483647 + 2147483647) % 2147483647)
            let idx: int = ((seed % (String.length (choices)) + (String.length (choices))) % (String.length (choices)))
            password <- Array.append password [|_substring choices idx (idx + 1)|]
            i <- i + 1
        __ret <- password
        raise Return
        __ret
    with
        | Return -> __ret
let rec unique_sorted (chars: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable chars = chars
    try
        let mutable uniq: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (chars)) do
            let ch: string = _idx chars (i)
            if not (Seq.contains ch uniq) then
                uniq <- Array.append uniq [|ch|]
            i <- i + 1
        let mutable j: int = 0
        while j < (Seq.length (uniq)) do
            let mutable k: int = j + 1
            let mutable min_idx: int = j
            while k < (Seq.length (uniq)) do
                if (_idx uniq (k)) < (_idx uniq (min_idx)) then
                    min_idx <- k
                k <- k + 1
            if min_idx <> j then
                let tmp: string = _idx uniq (j)
                uniq.[j] <- _idx uniq (min_idx)
                uniq.[min_idx] <- tmp
            j <- j + 1
        __ret <- uniq
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_key_list (passcode: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable passcode = passcode
    try
        let key_list_options: string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n"
        let breakpoints: string array = unique_sorted (passcode)
        let mutable keys_l: string array = [||]
        let mutable temp_list: string array = [||]
        let mutable i: int = 0
        while i < (String.length (key_list_options)) do
            let ch: string = _substring key_list_options i (i + 1)
            temp_list <- Array.append temp_list [|ch|]
            if (Seq.contains ch breakpoints) || (i = ((String.length (key_list_options)) - 1)) then
                let mutable k: int = (Seq.length (temp_list)) - 1
                while k >= 0 do
                    keys_l <- Array.append keys_l [|_idx temp_list (k)|]
                    k <- k - 1
                temp_list <- Array.empty<string>
            i <- i + 1
        __ret <- keys_l
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_shift_key (passcode: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable passcode = passcode
    try
        let mutable codes: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (passcode)) do
            codes <- Array.append codes [|ord (_idx passcode (i))|]
            i <- i + 1
        codes <- neg_pos (codes)
        let mutable total: int = 0
        i <- 0
        while i < (Seq.length (codes)) do
            total <- total + (_idx codes (i))
            i <- i + 1
        if total > 0 then
            __ret <- total
            raise Return
        __ret <- Seq.length (passcode)
        raise Return
        __ret
    with
        | Return -> __ret
let rec new_cipher (passcode_str: string) =
    let mutable __ret : Cipher = Unchecked.defaultof<Cipher>
    let mutable passcode_str = passcode_str
    try
        let mutable passcode: string array = [||]
        if (String.length (passcode_str)) = 0 then
            passcode <- passcode_creator()
        else
            let mutable i: int = 0
            while i < (String.length (passcode_str)) do
                passcode <- Array.append passcode [|_substring passcode_str i (i + 1)|]
                i <- i + 1
        let key_list: string array = make_key_list (passcode)
        let shift_key: int = make_shift_key (passcode)
        __ret <- { passcode = passcode; key_list = key_list; shift_key = shift_key }
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_of (lst: string array) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable lst = lst
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if (_idx lst (i)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec encrypt (c: Cipher) (plaintext: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    let mutable plaintext = plaintext
    try
        let mutable encoded: string = ""
        let mutable i: int = 0
        let n: int = Seq.length (c.key_list)
        while i < (String.length (plaintext)) do
            let ch: string = _substring plaintext i (i + 1)
            let position: int = index_of (c.key_list) (ch)
            let mutable new_pos: int = (((position + (c.shift_key)) % n + n) % n)
            encoded <- encoded + (_idx (c.key_list) (new_pos))
            i <- i + 1
        __ret <- encoded
        raise Return
        __ret
    with
        | Return -> __ret
let rec decrypt (c: Cipher) (encoded_message: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    let mutable encoded_message = encoded_message
    try
        let mutable decoded: string = ""
        let mutable i: int = 0
        let n: int = Seq.length (c.key_list)
        while i < (String.length (encoded_message)) do
            let ch: string = _substring encoded_message i (i + 1)
            let position: int = index_of (c.key_list) (ch)
            let mutable new_pos: int = (((position - (c.shift_key)) % n + n) % n)
            if new_pos < 0 then
                new_pos <- new_pos + n
            decoded <- decoded + (_idx (c.key_list) (new_pos))
            i <- i + 1
        __ret <- decoded
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_end_to_end () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        let msg: string = "Hello, this is a modified Caesar cipher"
        let cip: Cipher = new_cipher ("")
        __ret <- decrypt (cip) (encrypt (cip) (msg))
        raise Return
        __ret
    with
        | Return -> __ret
let ssc: Cipher = new_cipher ("4PYIXyqeQZr44")
let mutable encoded: string = encrypt (ssc) ("Hello, this is a modified Caesar cipher")
printfn "%s" (encoded)
printfn "%s" (decrypt (ssc) (encoded))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
