// Generated 2025-08-02 11:21 +0700

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
let rec bubbleSort (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let mutable arr: int array = a
        let mutable itemCount: int = (Seq.length (arr)) - 1
        try
            while true do
                try
                    let mutable hasChanged: bool = false
                    let mutable index: int = 0
                    while index < itemCount do
                        if (arr.[index]) > (arr.[index + 1]) then
                            let tmp: int = arr.[index]
                            arr.[index] <- arr.[index + 1]
                            arr.[index + 1] <- tmp
                            hasChanged <- true
                        index <- index + 1
                    if not hasChanged then
                        raise Break
                    itemCount <- itemCount - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let mutable list: int array = [|31; 41; 59; 26; 53; 58; 97; 93; 23; 84|]
printfn "%s" ("unsorted: " + (("[" + (unbox<string> (String.concat (" ") (Array.toList (Array.map string (list)))))) + "]"))
list <- bubbleSort (list)
printfn "%s" ("sorted!  " + (("[" + (unbox<string> (String.concat (" ") (Array.toList (Array.map string (list)))))) + "]"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
