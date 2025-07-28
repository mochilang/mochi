// Generated 2025-07-28 07:48 +0700

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
type Eatable = {
    eat: unit -> unit
}
type Foodbox = {
    items: PeelFirst array
}
type PeelFirst = {
    value: string
}
let rec peelFirstEat (p: PeelFirst) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable p = p
    try
        printfn "%s" (("mm, that " + (p.value)) + " was good!")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable fb: Foodbox = { items = [|{ value = "banana" }; { value = "mango" }|] }
        let mutable f0: PeelFirst = (fb.items).[0]
        peelFirstEat f0
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
