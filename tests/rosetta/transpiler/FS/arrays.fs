// Generated 2025-07-25 22:14 +0700

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
let rec listStr (xs: int array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable xs = xs
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (int (Array.length xs)) do
            s <- s + (string (xs.[i]))
            if (i + 1) < (int (Array.length xs)) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable a: int array = [|0; 0; 0; 0; 0|]
printfn "%s" ("len(a) = " + (string (Array.length a)))
printfn "%s" ("a = " + (unbox<string> (listStr a)))
a.[0] <- 3
printfn "%s" ("a = " + (unbox<string> (listStr a)))
printfn "%s" ("a[0] = " + (string (a.[0])))
let mutable s = Array.sub a 0 (4 - 0)
let mutable cap_s: int = 5
printfn "%s" ("s = " + (unbox<string> (listStr s)))
printfn "%s" ((("len(s) = " + (string (Array.length s))) + "  cap(s) = ") + (string cap_s))
s <- Array.sub a 0 (5 - 0)
printfn "%s" ("s = " + (unbox<string> (listStr s)))
a.[0] <- 22
s.[0] <- 22
printfn "%s" ("a = " + (unbox<string> (listStr a)))
printfn "%s" ("s = " + (unbox<string> (listStr s)))
s <- Array.append s [|4|]
s <- Array.append s [|5|]
s <- Array.append s [|6|]
cap_s <- 10
printfn "%s" ("s = " + (unbox<string> (listStr s)))
printfn "%s" ((("len(s) = " + (string (Array.length s))) + "  cap(s) = ") + (string cap_s))
a.[4] <- -1
printfn "%s" ("a = " + (unbox<string> (listStr a)))
printfn "%s" ("s = " + (unbox<string> (listStr s)))
s <- [||]
for i in 0 .. (8 - 1) do
    s <- Array.append s [|0|]
cap_s <- 8
printfn "%s" ("s = " + (unbox<string> (listStr s)))
printfn "%s" ((("len(s) = " + (string (Array.length s))) + "  cap(s) = ") + (string cap_s))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
