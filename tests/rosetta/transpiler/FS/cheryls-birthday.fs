// Generated 2025-07-27 23:45 +0700

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
type Birthday = {
    month: int
    day: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec monthUnique (b: Birthday) (list: Birthday array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    let mutable list = list
    try
        let mutable c: int = 0
        for x in list do
            if (x.month) = (b.month) then
                c <- c + 1
        __ret <- c = 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec dayUnique (b: Birthday) (list: Birthday array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    let mutable list = list
    try
        let mutable c: int = 0
        for x in list do
            if (x.day) = (b.day) then
                c <- c + 1
        __ret <- c = 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec monthWithUniqueDay (b: Birthday) (list: Birthday array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    let mutable list = list
    try
        for x in list do
            if ((x.month) = (b.month)) && (dayUnique x list) then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec bstr (b: Birthday) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        let months: string array = [|""; "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"|]
        __ret <- ((unbox<string> (months.[b.month])) + " ") + (string (b.day))
        raise Return
        __ret
    with
        | Return -> __ret
let mutable choices: Birthday array = [|{ month = 5; day = 15 }; { month = 5; day = 16 }; { month = 5; day = 19 }; { month = 6; day = 17 }; { month = 6; day = 18 }; { month = 7; day = 14 }; { month = 7; day = 16 }; { month = 8; day = 14 }; { month = 8; day = 15 }; { month = 8; day = 17 }|]
let mutable filtered: Birthday array = [||]
for bd in choices do
    if not (monthUnique bd choices) then
        filtered <- unbox<Birthday array> (Array.append filtered [|bd|])
let mutable filtered2: Birthday array = [||]
for bd in filtered do
    if not (monthWithUniqueDay bd filtered) then
        filtered2 <- unbox<Birthday array> (Array.append filtered2 [|bd|])
let mutable filtered3: Birthday array = [||]
for bd in filtered2 do
    if dayUnique bd filtered2 then
        filtered3 <- unbox<Birthday array> (Array.append filtered3 [|bd|])
let mutable filtered4: Birthday array = [||]
for bd in filtered3 do
    if monthUnique bd filtered3 then
        filtered4 <- unbox<Birthday array> (Array.append filtered4 [|bd|])
if (unbox<int> (Array.length filtered4)) = 1 then
    printfn "%s" ("Cheryl's birthday is " + (bstr (unbox<Birthday> (filtered4.[0]))))
else
    printfn "%s" "Something went wrong!"
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
