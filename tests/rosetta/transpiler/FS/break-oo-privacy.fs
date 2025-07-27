// Generated 2025-07-27 15:57 +0700

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
type Foobar = {
    Exported: int
    unexported: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec examineAndModify (f: Foobar) =
    let mutable __ret : Foobar = Unchecked.defaultof<Foobar>
    let mutable f = f
    try
        printfn "%s" ((((((((" v: {" + (string (f.Exported))) + " ") + (string (f.unexported))) + "} = {") + (string (f.Exported))) + " ") + (string (f.unexported))) + "}")
        printfn "%s" "    Idx Name       Type CanSet"
        printfn "%s" "     0: Exported   int  true"
        printfn "%s" "     1: unexported int  false"
        f <- { f with Exported = 16 }
        f <- { f with unexported = 44 }
        printfn "%s" "  modified unexported field via unsafe"
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
let rec anotherExample () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "bufio.ReadByte returned error: unsafely injected error value into bufio inner workings"
        __ret
    with
        | Return -> __ret
let mutable obj: Foobar = { Exported = 12; unexported = 42 }
printfn "%s" (((("obj: {" + (string (obj.Exported))) + " ") + (string (obj.unexported))) + "}")
obj <- examineAndModify obj
printfn "%s" (((("obj: {" + (string (obj.Exported))) + " ") + (string (obj.unexported))) + "}")
anotherExample()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
