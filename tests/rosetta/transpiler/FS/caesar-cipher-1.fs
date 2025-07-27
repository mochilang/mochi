// Generated 2025-07-27 15:57 +0700

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

let rec indexOf (s: string) (ch: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (String.length s) do
            if (_substring s i (i + 1)) = ch then
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
        let mutable idx: int = indexOf upper ch
        if idx >= 0 then
            __ret <- 65 + idx
            raise Return
        idx <- indexOf lower ch
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
            __ret <- upper.Substring(n - 65, (n - 64) - (n - 65))
            raise Return
        if (n >= 97) && (n < 123) then
            __ret <- lower.Substring(n - 97, (n - 96) - (n - 97))
            raise Return
        __ret <- "?"
        raise Return
        __ret
    with
        | Return -> __ret
and shiftRune (r: string) (k: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable r = r
    let mutable k = k
    try
        if (r >= "a") && (r <= "z") then
            __ret <- chr ((unbox<int> (((((unbox<int> ((unbox<int> (ord r)) - 97)) + k) % 26 + 26) % 26))) + 97)
            raise Return
        if (r >= "A") && (r <= "Z") then
            __ret <- chr ((unbox<int> (((((unbox<int> ((unbox<int> (ord r)) - 65)) + k) % 26 + 26) % 26))) + 65)
            raise Return
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and encipher (s: string) (k: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable k = k
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            out <- out + (unbox<string> (shiftRune (s.Substring(i, (i + 1) - i)) k))
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and decipher (s: string) (k: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable k = k
    try
        __ret <- encipher s ((((26 - (((k % 26 + 26) % 26))) % 26 + 26) % 26))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let pt: string = "The five boxing wizards jump quickly"
        printfn "%s" ("Plaintext: " + pt)
        for key in [|0; 1; 7; 25; 26|] do
            try
                if ((unbox<int> key) < 1) || ((unbox<int> key) > 25) then
                    printfn "%s" (("Key " + (string key)) + " invalid")
                    raise Continue
                let ct: string = encipher pt (unbox<int> key)
                printfn "%s" ("Key " + (string key))
                printfn "%s" ("  Enciphered: " + ct)
                printfn "%s" ("  Deciphered: " + (unbox<string> (decipher ct (unbox<int> key))))
            with
            | Break -> ()
            | Continue -> ()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
