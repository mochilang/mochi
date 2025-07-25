// Generated 2025-07-25 12:29 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec validComb (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) (g: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    let mutable d = d
    let mutable e = e
    let mutable f = f
    let mutable g = g
    try
        let square1: int = a + b
        let square2: int = (b + c) + d
        let square3: int = (d + e) + f
        let square4: int = f + g
        __ret <- ((square1 = square2) && (square2 = square3)) && (square3 = square4)
        raise Return
        __ret
    with
        | Return -> __ret
let rec isUnique (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) (g: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    let mutable d = d
    let mutable e = e
    let mutable f = f
    let mutable g = g
    try
        let mutable nums: int array = [|a; b; c; d; e; f; g|]
        let mutable i: int = 0
        while i < (Array.length nums) do
            let mutable j: int = i + 1
            while j < (Array.length nums) do
                if (nums.[i]) = (nums.[j]) then
                    __ret <- false
                    raise Return
                j <- j + 1
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec getCombs (low: int) (high: int) (unique: bool) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable low = low
    let mutable high = high
    let mutable unique = unique
    try
        let mutable valid = [||]
        let mutable count: int = 0
        for b in low .. ((high + 1) - 1) do
            try
                for c in low .. ((high + 1) - 1) do
                    try
                        for d in low .. ((high + 1) - 1) do
                            try
                                let s = (b + c) + d
                                for e in low .. ((high + 1) - 1) do
                                    try
                                        for f in low .. ((high + 1) - 1) do
                                            try
                                                let a = s - b
                                                let g = s - f
                                                if (a < low) || (a > high) then
                                                    raise Continue
                                                if (g < low) || (g > high) then
                                                    raise Continue
                                                if ((d + e) + f) <> s then
                                                    raise Continue
                                                if (f + g) <> s then
                                                    raise Continue
                                                if (not unique) || (isUnique a b c d e f g) then
                                                    valid <- Array.append valid [|[|a; b; c; d; e; f; g|]|]
                                                    count <- count + 1
                                            with
                                            | Break -> ()
                                            | Continue -> ()
                                    with
                                    | Break -> ()
                                    | Continue -> ()
                            with
                            | Break -> ()
                            | Continue -> ()
                    with
                    | Break -> ()
                    | Continue -> ()
            with
            | Break -> ()
            | Continue -> ()
        __ret <- Map.ofList [("count", box count); ("list", box valid)]
        raise Return
        __ret
    with
        | Return -> __ret
let r1 = getCombs 1 7 true
printfn "%s" ((string (r1.["count"])) + " unique solutions in 1 to 7")
printfn "%A" (r1.["list"])
let r2 = getCombs 3 9 true
printfn "%s" ((string (r2.["count"])) + " unique solutions in 3 to 9")
printfn "%A" (r2.["list"])
let r3 = getCombs 0 9 false
printfn "%s" ((string (r3.["count"])) + " non-unique solutions in 0 to 9")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
