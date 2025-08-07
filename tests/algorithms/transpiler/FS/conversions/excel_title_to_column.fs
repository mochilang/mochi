// Generated 2025-08-07 10:31 +0700

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
let letters: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec excel_title_to_column (title: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable title = title
    try
        let mutable result: int = 0
        let mutable i: int = 0
        try
            while i < (String.length (title)) do
                try
                    let ch: string = title.Substring(i, (i + 1) - i)
                    let mutable value: int = 0
                    let mutable idx: int = 0
                    let mutable found: bool = false
                    try
                        while idx < (String.length (letters)) do
                            try
                                if (letters.Substring(idx, (idx + 1) - idx)) = ch then
                                    value <- idx + 1
                                    found <- true
                                    raise Break
                                idx <- idx + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not found then
                        failwith ("title must contain only uppercase A-Z")
                    result <- (result * 26) + value
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
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
        printfn "%d" (excel_title_to_column ("A"))
        printfn "%d" (excel_title_to_column ("B"))
        printfn "%d" (excel_title_to_column ("AB"))
        printfn "%d" (excel_title_to_column ("Z"))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
