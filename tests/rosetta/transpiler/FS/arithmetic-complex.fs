// Generated 2025-07-25 14:38 +0000

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
type Complex = {
    re: float
    im: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec add (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { re = (a.re) + (b.re); im = (a.im) + (b.im) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec mul (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { re = ((a.re) * (b.re)) - ((a.im) * (b.im)); im = ((a.re) * (b.im)) + ((a.im) * (b.re)) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec neg (a: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    try
        __ret <- { re = -(a.re); im = -(a.im) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec inv (a: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    try
        let denom: float = ((a.re) * (a.re)) + ((a.im) * (a.im))
        __ret <- { re = (a.re) / denom; im = (-(a.im)) / denom }
        raise Return
        __ret
    with
        | Return -> __ret
let rec conj (a: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    try
        __ret <- { re = a.re; im = -(a.im) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec cstr (a: Complex) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    try
        let mutable s: string = "(" + (string (a.re))
        if (a.im) >= (float 0) then
            s <- ((s + "+") + (string (a.im))) + "i)"
        else
            s <- (s + (string (a.im))) + "i)"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let a: Complex = { re = 1.0; im = 1.0 }
let b: Complex = { re = 3.14159; im = 1.25 }
printfn "%s" ("a:       " + (unbox<string> (cstr a)))
printfn "%s" ("b:       " + (unbox<string> (cstr b)))
printfn "%s" ("a + b:   " + (unbox<string> (cstr (unbox<Complex> (add a b)))))
printfn "%s" ("a * b:   " + (unbox<string> (cstr (unbox<Complex> (mul a b)))))
printfn "%s" ("-a:      " + (unbox<string> (cstr (unbox<Complex> (neg a)))))
printfn "%s" ("1 / a:   " + (unbox<string> (cstr (unbox<Complex> (inv a)))))
printfn "%s" ("a̅:       " + (unbox<string> (cstr (unbox<Complex> (conj a)))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
