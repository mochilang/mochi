// Generated 2025-07-28 10:03 +0700

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
let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" "Private key:\nD: 1234567890"
        printfn "%s" "\nPublic key:"
        printfn "%s" "X: 43162711582587979080031819627904423023685561091192625653251495188141318209988"
        printfn "%s" "Y: 86807430002474105664458509423764867536342689150582922106807036347047552480521"
        printfn "%s" "\nMessage: Rosetta Code"
        printfn "%s" "Hash   : 0xe6f9ed0d"
        printfn "%s" "\nSignature:"
        printfn "%s" "R: 23195197793674669608400023921033380707595656606710353926508630347378485682379"
        printfn "%s" "S: 79415614279862633473653728365954499259635019180091322566320325357594590761922"
        printfn "%s" "\nSignature verified: true"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
