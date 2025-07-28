// Generated 2025-07-28 07:48 +0700

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
type Field = {
    s: bool array array
    w: int
    h: int
}
type Life = {
    a: Field
    b: Field
    w: int
    h: int
}
let mutable seed: int = 1
let rec randN (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        seed <- ((((seed * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- ((seed % n + n) % n)
        raise Return
        __ret
    with
        | Return -> __ret
and newField (w: int) (h: int) =
    let mutable __ret : Field = Unchecked.defaultof<Field>
    let mutable w = w
    let mutable h = h
    try
        let mutable rows: bool array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: bool array = [||]
            let mutable x: int = 0
            while x < w do
                row <- Array.append row [|false|]
                x <- x + 1
            rows <- Array.append rows [|row|]
            y <- y + 1
        __ret <- { s = rows; w = w; h = h }
        raise Return
        __ret
    with
        | Return -> __ret
and setCell (f: Field) (x: int) (y: int) (b: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable f = f
    let mutable x = x
    let mutable y = y
    let mutable b = b
    try
        let mutable rows: bool array array = f.s
        let mutable row: bool array = rows.[y]
        row.[x] <- b
        rows.[y] <- row
        f <- { f with s = rows }
        __ret
    with
        | Return -> __ret
and state (f: Field) (x: int) (y: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable f = f
    let mutable x = x
    let mutable y = y
    try
        while y < 0 do
            y <- y + (f.h)
        while x < 0 do
            x <- x + (f.w)
        __ret <- ((f.s).[((y % (f.h) + (f.h)) % (f.h))]).[((x % (f.w) + (f.w)) % (f.w))]
        raise Return
        __ret
    with
        | Return -> __ret
and nextState (f: Field) (x: int) (y: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable f = f
    let mutable x = x
    let mutable y = y
    try
        let mutable count: int = 0
        let mutable dy: int = -1
        while dy <= 1 do
            let mutable dx: int = -1
            while dx <= 1 do
                if (not ((dx = 0) && (dy = 0))) && (unbox<bool> (state f (x + dx) (y + dy))) then
                    count <- count + 1
                dx <- dx + 1
            dy <- dy + 1
        __ret <- (count = 3) || ((count = 2) && (unbox<bool> (state f x y)))
        raise Return
        __ret
    with
        | Return -> __ret
and newLife (w: int) (h: int) =
    let mutable __ret : Life = Unchecked.defaultof<Life>
    let mutable w = w
    let mutable h = h
    try
        let mutable a: Field = newField w h
        let mutable i: int = 0
        while i < ((w * h) / 2) do
            setCell a (randN w) (randN h) true
            i <- i + 1
        __ret <- { a = a; b = newField w h; w = w; h = h }
        raise Return
        __ret
    with
        | Return -> __ret
and step (l: Life) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable l = l
    try
        let mutable y: int = 0
        while y < (l.h) do
            let mutable x: int = 0
            while x < (l.w) do
                setCell (l.b) x y (nextState (l.a) x y)
                x <- x + 1
            y <- y + 1
        let mutable tmp: Field = l.a
        l <- { l with a = l.b }
        l <- { l with b = tmp }
        __ret
    with
        | Return -> __ret
and lifeString (l: Life) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable l = l
    try
        let mutable out: string = ""
        let mutable y: int = 0
        while y < (l.h) do
            let mutable x: int = 0
            while x < (l.w) do
                if state (l.a) x y then
                    out <- out + "*"
                else
                    out <- out + " "
                x <- x + 1
            out <- out + "\n"
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable l: Life = newLife 80 15
        let mutable i: int = 0
        while i < 300 do
            step l
            printfn "%s" "\f"
            printfn "%s" (lifeString l)
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
