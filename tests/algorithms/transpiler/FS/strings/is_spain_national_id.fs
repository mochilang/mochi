// Generated 2025-08-11 15:32 +0700

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

let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let DIGITS: string = "0123456789"
let UPPER: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let LOWER: string = "abcdefghijklmnopqrstuvwxyz"
let LOOKUP_LETTERS: string = "TRWAGMYFPDXBNJZSQVHLCKE"
let ERROR_MSG: string = "Input must be a string of 8 numbers plus letter"
let rec to_upper (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable res: string = ""
        let mutable i: int = 0
        try
            while i < (String.length (s)) do
                try
                    let ch: string = string (s.[i])
                    let mutable j: int = 0
                    let mutable converted: string = ch
                    try
                        while j < (String.length (LOWER)) do
                            try
                                if (string (LOWER.[j])) = ch then
                                    converted <- string (UPPER.[j])
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    res <- res + converted
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and is_digit (ch: string) =
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
and clean_id (spanish_id: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable spanish_id = spanish_id
    try
        let upper_id: string = to_upper (spanish_id)
        let mutable cleaned: string = ""
        let mutable i: int = 0
        while i < (String.length (upper_id)) do
            let ch: string = string (upper_id.[i])
            if ch <> "-" then
                cleaned <- cleaned + ch
            i <- i + 1
        __ret <- cleaned
        raise Return
        __ret
    with
        | Return -> __ret
and is_spain_national_id (spanish_id: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable spanish_id = spanish_id
    try
        let sid: string = clean_id (spanish_id)
        if (String.length (sid)) <> 9 then
            failwith (ERROR_MSG)
        let mutable i: int = 0
        while i < 8 do
            if not (is_digit (string (sid.[i]))) then
                failwith (ERROR_MSG)
            i <- i + 1
        let number: int = int (_substring sid 0 8)
        let letter: string = string (sid.[8])
        if is_digit (letter) then
            failwith (ERROR_MSG)
        let expected: string = string (LOOKUP_LETTERS.[((number % 23 + 23) % 23)])
        __ret <- letter = expected
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%b" (is_spain_national_id ("12345678Z"))
        printfn "%b" (is_spain_national_id ("12345678z"))
        printfn "%b" (is_spain_national_id ("12345678x"))
        printfn "%b" (is_spain_national_id ("12345678I"))
        printfn "%b" (is_spain_national_id ("12345678-Z"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
