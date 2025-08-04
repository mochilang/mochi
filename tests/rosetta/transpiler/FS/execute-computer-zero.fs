// Generated 2025-08-04 20:03 +0700

exception Break
exception Continue

exception Return
let mutable __ret = ()

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec floorMod (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable r: int = ((a % b + b) % b)
        if r < 0 then
            r <- r + b
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and run (bc: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable bc = bc
    try
        let mutable acc: int = 0
        let mutable pc: int = 0
        try
            while pc < 32 do
                try
                    let op: int = (_idx bc pc) / 32
                    let arg: int = (((_idx bc pc) % 32 + 32) % 32)
                    pc <- pc + 1
                    if op = 0 then ()
                    else
                        if op = 1 then
                            acc <- _idx bc arg
                        else
                            if op = 2 then
                                bc.[arg] <- acc
                            else
                                if op = 3 then
                                    acc <- floorMod (acc + (_idx bc arg)) (256)
                                else
                                    if op = 4 then
                                        acc <- floorMod (acc - (_idx bc arg)) (256)
                                    else
                                        if op = 5 then
                                            if acc = 0 then
                                                pc <- arg
                                        else
                                            if op = 6 then
                                                pc <- arg
                                            else
                                                if op = 7 then
                                                    raise Break
                                                else
                                                    raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- acc
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let programs: int array array = [|[|35; 100; 224; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|44; 106; 76; 43; 141; 75; 168; 192; 44; 224; 8; 7; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|46; 79; 109; 78; 47; 77; 48; 145; 171; 80; 192; 46; 224; 1; 1; 0; 8; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|45; 111; 69; 112; 71; 0; 78; 0; 171; 79; 192; 46; 224; 32; 0; 28; 1; 0; 0; 0; 6; 0; 2; 26; 5; 20; 3; 30; 1; 22; 4; 24|]; [|35; 132; 224; 0; 255; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|35; 132; 224; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|35; 100; 224; 1; 255; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]|]
        let mutable i: int = 0
        while i < (Seq.length (programs)) do
            let res: int = run (_idx programs i)
            printfn "%s" (string (res))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
