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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let abc: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let low_abc: string = "abcdefghijklmnopqrstuvwxyz"
let rotor1: string = "EGZWVONAHDCLFQMSIPJBYUKXTR"
let rotor2: string = "FOBHMDKEXQNRAULPGSJVTYICZW"
let rotor3: string = "ZJXESIUQLHAVRMDOYGTNFWPBKC"
let rotor4: string = "RMDJXFUWGISLHVTCQNKYPBEZOA"
let rotor5: string = "SGLCPQWZHKXAREONTFBVIYJUDM"
let rotor6: string = "HVSICLTYKQUBXDWAJZOMFGPREN"
let rotor7: string = "RZWQHFMVDBKICJLNTUXAGYPSOE"
let rotor8: string = "LFKIJODBEGAMQPXVUHYSTCZRWN"
let rotor9: string = "KOAEGVDHXPQZMLFTYWJNBRCIUS"
let reflector_pairs: string array = [|"AN"; "BO"; "CP"; "DQ"; "ER"; "FS"; "GT"; "HU"; "IV"; "JW"; "KX"; "LY"; "MZ"|]
let rec list_contains (xs: string array) (x: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (i)) = x then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and index_in_string (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length (s)) do
            if (_substring s i (i + 1)) = ch then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and contains_char (s: string) (ch: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    let mutable ch = ch
    try
        __ret <- (index_in_string (s) (ch)) >= 0
        raise Return
        __ret
    with
        | Return -> __ret
and to_uppercase (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = _substring s i (i + 1)
            let idx: int = index_in_string (low_abc) (ch)
            if idx >= 0 then
                res <- res + (_substring abc idx (idx + 1))
            else
                res <- res + ch
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and plugboard_map (pb: string array) (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable pb = pb
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (Seq.length (pb)) do
            let pair: string = _idx pb (i)
            let a: string = _substring pair 0 1
            let b: string = _substring pair 1 2
            if ch = a then
                __ret <- b
                raise Return
            if ch = b then
                __ret <- a
                raise Return
            i <- i + 1
        __ret <- ch
        raise Return
        __ret
    with
        | Return -> __ret
and reflector_map (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (Seq.length (reflector_pairs)) do
            let pair: string = _idx reflector_pairs (i)
            let a: string = _substring pair 0 1
            let b: string = _substring pair 1 2
            if ch = a then
                __ret <- b
                raise Return
            if ch = b then
                __ret <- a
                raise Return
            i <- i + 1
        __ret <- ch
        raise Return
        __ret
    with
        | Return -> __ret
and count_unique (xs: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable unique: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if not (list_contains (unique) (_idx xs (i))) then
                unique <- Array.append unique [|_idx xs (i)|]
            i <- i + 1
        __ret <- Seq.length (unique)
        raise Return
        __ret
    with
        | Return -> __ret
and build_plugboard (pbstring: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable pbstring = pbstring
    try
        if (String.length (pbstring)) = 0 then
            __ret <- Array.empty<string>
            raise Return
        if ((((String.length (pbstring)) % 2 + 2) % 2)) <> 0 then
            failwith (("Odd number of symbols(" + (_str (String.length (pbstring)))) + ")")
        let mutable pbstring_nospace: string = ""
        let mutable i: int = 0
        while i < (String.length (pbstring)) do
            let ch: string = _substring pbstring i (i + 1)
            if ch <> " " then
                pbstring_nospace <- pbstring_nospace + ch
            i <- i + 1
        let mutable seen: string array = [||]
        i <- 0
        while i < (String.length (pbstring_nospace)) do
            let ch: string = _substring pbstring_nospace i (i + 1)
            if not (contains_char (abc) (ch)) then
                failwith (("'" + ch) + "' not in list of symbols")
            if list_contains (seen) (ch) then
                failwith (("Duplicate symbol(" + ch) + ")")
            seen <- Array.append seen [|ch|]
            i <- i + 1
        let mutable pb: string array = [||]
        i <- 0
        while i < ((String.length (pbstring_nospace)) - 1) do
            let a: string = _substring pbstring_nospace i (i + 1)
            let b: string = _substring pbstring_nospace (i + 1) (i + 2)
            pb <- Array.append pb [|a + b|]
            i <- i + 2
        __ret <- pb
        raise Return
        __ret
    with
        | Return -> __ret
and validator (rotpos: int array) (rotsel: string array) (pb: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable rotpos = rotpos
    let mutable rotsel = rotsel
    let mutable pb = pb
    try
        if (count_unique (rotsel)) < 3 then
            failwith (("Please use 3 unique rotors (not " + (_str (count_unique (rotsel)))) + ")")
        if (Seq.length (rotpos)) <> 3 then
            failwith ("Rotor position must have 3 values")
        let r1: int = _idx rotpos (0)
        let r2: int = _idx rotpos (1)
        let r3: int = _idx rotpos (2)
        if not ((0 < r1) && (r1 <= (String.length (abc)))) then
            failwith (("First rotor position is not within range of 1..26 (" + (_str (r1))) + ")")
        if not ((0 < r2) && (r2 <= (String.length (abc)))) then
            failwith (("Second rotor position is not within range of 1..26 (" + (_str (r2))) + ")")
        if not ((0 < r3) && (r3 <= (String.length (abc)))) then
            failwith (("Third rotor position is not within range of 1..26 (" + (_str (r3))) + ")")
        __ret
    with
        | Return -> __ret
and enigma (text: string) (rotor_position: int array) (rotor_selection: string array) (plugb: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    let mutable rotor_position = rotor_position
    let mutable rotor_selection = rotor_selection
    let mutable plugb = plugb
    try
        let up_text: string = to_uppercase (text)
        let up_pb: string = to_uppercase (plugb)
        validator (rotor_position) (rotor_selection) (up_pb)
        let plugboard: string array = build_plugboard (up_pb)
        let mutable rotorpos1: int = (_idx rotor_position (0)) - 1
        let mutable rotorpos2: int = (_idx rotor_position (1)) - 1
        let mutable rotorpos3: int = (_idx rotor_position (2)) - 1
        let rotor_a: string = _idx rotor_selection (0)
        let rotor_b: string = _idx rotor_selection (1)
        let rotor_c: string = _idx rotor_selection (2)
        let mutable result: string = ""
        let mutable i: int = 0
        while i < (String.length (up_text)) do
            let mutable symbol: string = _substring up_text i (i + 1)
            if contains_char (abc) (symbol) then
                symbol <- plugboard_map (plugboard) (symbol)
                let mutable index: int = (index_in_string (abc) (symbol)) + rotorpos1
                symbol <- _substring rotor_a (((index % (String.length (abc)) + (String.length (abc))) % (String.length (abc)))) ((((index % (String.length (abc)) + (String.length (abc))) % (String.length (abc)))) + 1)
                index <- (index_in_string (abc) (symbol)) + rotorpos2
                symbol <- _substring rotor_b (((index % (String.length (abc)) + (String.length (abc))) % (String.length (abc)))) ((((index % (String.length (abc)) + (String.length (abc))) % (String.length (abc)))) + 1)
                index <- (index_in_string (abc) (symbol)) + rotorpos3
                symbol <- _substring rotor_c (((index % (String.length (abc)) + (String.length (abc))) % (String.length (abc)))) ((((index % (String.length (abc)) + (String.length (abc))) % (String.length (abc)))) + 1)
                symbol <- reflector_map (symbol)
                index <- (index_in_string (rotor_c) (symbol)) - rotorpos3
                if index < 0 then
                    index <- index + (String.length (abc))
                symbol <- _substring abc index (index + 1)
                index <- (index_in_string (rotor_b) (symbol)) - rotorpos2
                if index < 0 then
                    index <- index + (String.length (abc))
                symbol <- _substring abc index (index + 1)
                index <- (index_in_string (rotor_a) (symbol)) - rotorpos1
                if index < 0 then
                    index <- index + (String.length (abc))
                symbol <- _substring abc index (index + 1)
                symbol <- plugboard_map (plugboard) (symbol)
                rotorpos1 <- rotorpos1 + 1
                if rotorpos1 >= (String.length (abc)) then
                    rotorpos1 <- 0
                    rotorpos2 <- rotorpos2 + 1
                if rotorpos2 >= (String.length (abc)) then
                    rotorpos2 <- 0
                    rotorpos3 <- rotorpos3 + 1
                if rotorpos3 >= (String.length (abc)) then
                    rotorpos3 <- 0
            result <- result + symbol
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let message: string = "This is my Python script that emulates the Enigma machine from WWII."
        let rotor_pos: int array = [|1; 1; 1|]
        let mutable pb: string = "pictures"
        let rotor_sel: string array = [|rotor2; rotor4; rotor8|]
        let en: string = enigma (message) (rotor_pos) (rotor_sel) (pb)
        printfn "%s" ("Encrypted message: " + en)
        printfn "%s" ("Decrypted message: " + (enigma (en) (rotor_pos) (rotor_sel) (pb)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
