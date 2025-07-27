// Generated 2025-07-27 23:45 +0700

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

type Anon1 = {
    ``base``: int
    ``begin``: string
    ``end``: string
    kaprekar: string array
}
type Anon2 = {
    ``base``: int
    ``begin``: string
    ``end``: string
    kaprekar: string array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec parseIntBase (s: string) (``base``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable s = s
    let mutable ``base`` = ``base``
    try
        let digits: string = "0123456789abcdefghijklmnopqrstuvwxyz"
        let mutable n: int = 0
        let mutable i: int = 0
        try
            while i < (String.length s) do
                let mutable j: int = 0
                let mutable v: int = 0
                try
                    while j < (String.length digits) do
                        if (_substring digits j (j + 1)) = (s.Substring(i, (i + 1) - i)) then
                            v <- j
                            raise Break
                        j <- j + 1
                with
                | Break -> ()
                | Continue -> ()
                n <- (n * ``base``) + v
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
let rec intToBase (n: int) (``base``: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable ``base`` = ``base``
    try
        let digits: string = "0123456789abcdefghijklmnopqrstuvwxyz"
        if n = 0 then
            __ret <- "0"
            raise Return
        let mutable out: string = ""
        let mutable v: int = n
        while v > 0 do
            let d: int = ((v % ``base`` + ``base``) % ``base``)
            out <- (digits.Substring(d, (d + 1) - d)) + out
            v <- v / ``base``
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec subset (``base``: int) (``begin``: string) (``end``: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable ``base`` = ``base``
    let mutable ``begin`` = ``begin``
    let mutable ``end`` = ``end``
    try
        let mutable b: int = parseIntBase ``begin`` ``base``
        let mutable e: int = parseIntBase ``end`` ``base``
        let mutable out: string array = [||]
        let mutable k: int = b
        while k <= e do
            let ks: string = intToBase k ``base``
            let ``mod``: int = ``base`` - 1
            let r1: int = (((parseIntBase ks ``base``) % ``mod`` + ``mod``) % ``mod``)
            let r2: int = ((((parseIntBase ks ``base``) * (parseIntBase ks ``base``)) % ``mod`` + ``mod``) % ``mod``)
            if r1 = r2 then
                out <- unbox<string array> (Array.append out [|ks|])
            k <- k + 1
        __ret <- unbox<string array> out
        raise Return
        __ret
    with
        | Return -> __ret
let testCases: Anon2 array = [|{ ``base`` = 10; ``begin`` = "1"; ``end`` = "100"; kaprekar = [|"1"; "9"; "45"; "55"; "99"|] }; { ``base`` = 17; ``begin`` = "10"; ``end`` = "gg"; kaprekar = [|"3d"; "d4"; "gg"|] }|]
let mutable idx: int = 0
try
    while idx < (unbox<int> (Array.length testCases)) do
        let tc: Anon2 = testCases.[idx]
        printfn "%s" (((((("\nTest case base = " + (string (tc.``base``))) + ", begin = ") + (tc.``begin``)) + ", end = ") + (tc.``end``)) + ":")
        let s: string array = subset (tc.``base``) (tc.``begin``) (tc.``end``)
        printfn "%s" ("Subset:  " + (string s))
        printfn "%s" ("Kaprekar:" + (string (tc.kaprekar)))
        let mutable sx: int = 0
        let mutable valid: bool = true
        let mutable i: int = 0
        try
            while i < (Seq.length (tc.kaprekar)) do
                let k: string = (tc.kaprekar).[i]
                let mutable found: bool = false
                try
                    while sx < (unbox<int> (Array.length s)) do
                        if (unbox<string> (s.[sx])) = k then
                            found <- true
                            sx <- sx + 1
                            raise Break
                        sx <- sx + 1
                with
                | Break -> ()
                | Continue -> ()
                if not found then
                    printfn "%s" (("Fail:" + k) + " not in subset")
                    valid <- false
                    raise Break
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        if valid then
            printfn "%s" "Valid subset."
        idx <- idx + 1
with
| Break -> ()
| Continue -> ()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
