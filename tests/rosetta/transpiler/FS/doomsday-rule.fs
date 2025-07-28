// Generated 2025-07-28 10:03 +0700

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
let rec parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length str) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- int ((n * 10) + (int (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>)))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
let days: string array = [|"Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday"|]
let rec anchorDay (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    try
        __ret <- (((((2 + (5 * (((y % 4 + 4) % 4)))) + (4 * (((y % 100 + 100) % 100)))) + (6 * (((y % 400 + 400) % 400)))) % 7 + 7) % 7)
        raise Return
        __ret
    with
        | Return -> __ret
and isLeapYear (y: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable y = y
    try
        __ret <- ((((y % 4 + 4) % 4)) = 0) && (((((y % 100 + 100) % 100)) <> 0) || ((((y % 400 + 400) % 400)) = 0))
        raise Return
        __ret
    with
        | Return -> __ret
let firstDaysCommon: int array = [|3; 7; 7; 4; 2; 6; 4; 1; 5; 3; 7; 5|]
let firstDaysLeap: int array = [|4; 1; 7; 4; 2; 6; 4; 1; 5; 3; 7; 5|]
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let dates: string array = [|"1800-01-06"; "1875-03-29"; "1915-12-07"; "1970-12-23"; "2043-05-14"; "2077-02-12"; "2101-04-02"|]
        printfn "%s" "Days of week given by Doomsday rule:"
        for date in dates do
            let y: int = parseIntStr (date.Substring(0, 4 - 0))
            let m: int = (int (parseIntStr (date.Substring(5, 7 - 5)))) - 1
            let d: int = parseIntStr (date.Substring(8, 10 - 8))
            let a: int = anchorDay y
            let mutable f: int = firstDaysCommon.[m]
            if isLeapYear y then
                f <- firstDaysLeap.[m]
            let mutable w: int = d - f
            if w < 0 then
                w <- 7 + w
            let dow: int = (((a + w) % 7 + 7) % 7)
            printfn "%s" ((date + " -> ") + (days.[dow]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
