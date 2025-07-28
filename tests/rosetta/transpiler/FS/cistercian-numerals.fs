// Generated 2025-07-28 01:10 +0700

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
let mutable n: string array array = [||]
let rec initN () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable i: int = 0
        while i < 15 do
            let mutable row: string array = [||]
            let mutable j: int = 0
            while j < 11 do
                row <- Array.append row [|" "|]
                j <- j + 1
            row.[5] <- "x"
            n <- Array.append n [|row|]
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec horiz (c1: int) (c2: int) (r: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable c1 = c1
    let mutable c2 = c2
    let mutable r = r
    try
        let mutable c: int = c1
        while c <= c2 do
            (n.[r]).[c] <- "x"
            c <- c + 1
        __ret
    with
        | Return -> __ret
let rec verti (r1: int) (r2: int) (c: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable r1 = r1
    let mutable r2 = r2
    let mutable c = c
    try
        let mutable r: int = r1
        while r <= r2 do
            (n.[r]).[c] <- "x"
            r <- r + 1
        __ret
    with
        | Return -> __ret
let rec diagd (c1: int) (c2: int) (r: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable c1 = c1
    let mutable c2 = c2
    let mutable r = r
    try
        let mutable c: int = c1
        while c <= c2 do
            (n.[(r + c) - c1]).[c] <- "x"
            c <- c + 1
        __ret
    with
        | Return -> __ret
let rec diagu (c1: int) (c2: int) (r: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable c1 = c1
    let mutable c2 = c2
    let mutable r = r
    try
        let mutable c: int = c1
        while c <= c2 do
            (n.[(r - c) + c1]).[c] <- "x"
            c <- c + 1
        __ret
    with
        | Return -> __ret
let mutable draw: Map<int, unit -> unit> = Map.ofList []
let rec initDraw () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        draw <- Map.add 1 (        fun () -> 
            horiz 6 10 0) draw
        draw <- Map.add 2 (        fun () -> 
            horiz 6 10 4) draw
        draw <- Map.add 3 (        fun () -> 
            diagd 6 10 0) draw
        draw <- Map.add 4 (        fun () -> 
            diagu 6 10 4) draw
        draw <- Map.add 5 (        fun () -> 
            (draw.[1] |> unbox<unit -> unit>) ()
            (draw.[4] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 6 (        fun () -> 
            verti 0 4 10) draw
        draw <- Map.add 7 (        fun () -> 
            (draw.[1] |> unbox<unit -> unit>) ()
            (draw.[6] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 8 (        fun () -> 
            (draw.[2] |> unbox<unit -> unit>) ()
            (draw.[6] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 9 (        fun () -> 
            (draw.[1] |> unbox<unit -> unit>) ()
            (draw.[8] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 10 (        fun () -> 
            horiz 0 4 0) draw
        draw <- Map.add 20 (        fun () -> 
            horiz 0 4 4) draw
        draw <- Map.add 30 (        fun () -> 
            diagu 0 4 4) draw
        draw <- Map.add 40 (        fun () -> 
            diagd 0 4 0) draw
        draw <- Map.add 50 (        fun () -> 
            (draw.[10] |> unbox<unit -> unit>) ()
            (draw.[40] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 60 (        fun () -> 
            verti 0 4 0) draw
        draw <- Map.add 70 (        fun () -> 
            (draw.[10] |> unbox<unit -> unit>) ()
            (draw.[60] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 80 (        fun () -> 
            (draw.[20] |> unbox<unit -> unit>) ()
            (draw.[60] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 90 (        fun () -> 
            (draw.[10] |> unbox<unit -> unit>) ()
            (draw.[80] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 100 (        fun () -> 
            horiz 6 10 14) draw
        draw <- Map.add 200 (        fun () -> 
            horiz 6 10 10) draw
        draw <- Map.add 300 (        fun () -> 
            diagu 6 10 14) draw
        draw <- Map.add 400 (        fun () -> 
            diagd 6 10 10) draw
        draw <- Map.add 500 (        fun () -> 
            (draw.[100] |> unbox<unit -> unit>) ()
            (draw.[400] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 600 (        fun () -> 
            verti 10 14 10) draw
        draw <- Map.add 700 (        fun () -> 
            (draw.[100] |> unbox<unit -> unit>) ()
            (draw.[600] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 800 (        fun () -> 
            (draw.[200] |> unbox<unit -> unit>) ()
            (draw.[600] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 900 (        fun () -> 
            (draw.[100] |> unbox<unit -> unit>) ()
            (draw.[800] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 1000 (        fun () -> 
            horiz 0 4 14) draw
        draw <- Map.add 2000 (        fun () -> 
            horiz 0 4 10) draw
        draw <- Map.add 3000 (        fun () -> 
            diagd 0 4 10) draw
        draw <- Map.add 4000 (        fun () -> 
            diagu 0 4 14) draw
        draw <- Map.add 5000 (        fun () -> 
            (draw.[1000] |> unbox<unit -> unit>) ()
            (draw.[4000] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 6000 (        fun () -> 
            verti 10 14 0) draw
        draw <- Map.add 7000 (        fun () -> 
            (draw.[1000] |> unbox<unit -> unit>) ()
            (draw.[6000] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 8000 (        fun () -> 
            (draw.[2000] |> unbox<unit -> unit>) ()
            (draw.[6000] |> unbox<unit -> unit>) ()) draw
        draw <- Map.add 9000 (        fun () -> 
            (draw.[1000] |> unbox<unit -> unit>) ()
            (draw.[8000] |> unbox<unit -> unit>) ()) draw
        __ret
    with
        | Return -> __ret
let rec printNumeral () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable i: int = 0
        while i < 15 do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < 11 do
                line <- (line + ((n.[i]).[j])) + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 1
        printfn "%s" ""
        __ret
    with
        | Return -> __ret
initDraw()
let numbers: int array = [|0; 1; 20; 300; 4000; 5555; 6789; 9999|]
for number in numbers do
    initN()
    printfn "%s" ((string number) + ":")
    let mutable num: int = number
    let thousands: int = num / 1000
    num <- ((num % 1000 + 1000) % 1000)
    let hundreds: int = num / 100
    num <- ((num % 100 + 100) % 100)
    let tens: int = num / 10
    let ones: int = ((num % 10 + 10) % 10)
    if thousands > 0 then
        (draw.[(thousands * 1000)] |> unbox<unit -> unit>) ()
    if hundreds > 0 then
        (draw.[(hundreds * 100)] |> unbox<unit -> unit>) ()
    if tens > 0 then
        (draw.[(tens * 10)] |> unbox<unit -> unit>) ()
    if ones > 0 then
        (draw.[ones] |> unbox<unit -> unit>) ()
    printNumeral()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
