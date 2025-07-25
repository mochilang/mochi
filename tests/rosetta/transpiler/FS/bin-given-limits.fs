// Generated 2025-07-26 04:38 +0700

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
let rec getBins (limits: int array) (data: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable limits = limits
    let mutable data = data
    try
        let mutable n: int = Array.length limits
        let mutable bins: int array = [||]
        let mutable i: int = 0
        while i < (n + 1) do
            bins <- Array.append bins [|0|]
            i <- i + 1
        let mutable j: int = 0
        try
            while j < (int (Array.length data)) do
                let d: int = data.[j]
                let mutable index: int = 0
                try
                    while index < (int (Array.length limits)) do
                        if d < (int (limits.[index])) then
                            raise Break
                        if d = (int (limits.[index])) then
                            index <- index + 1
                            raise Break
                        index <- index + 1
                with
                | Break -> ()
                | Continue -> ()
                bins.[index] <- (int (bins.[index])) + 1
                j <- j + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret <- bins
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        let mutable pad: int = width - (String.length s)
        let mutable out: string = ""
        let mutable i: int = 0
        while i < pad do
            out <- out + " "
            i <- i + 1
        __ret <- out + s
        raise Return
        __ret
    with
        | Return -> __ret
and printBins (limits: int array) (bins: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable limits = limits
    let mutable bins = bins
    try
        let mutable n: int = Array.length limits
        printfn "%s" ((("           < " + (unbox<string> (padLeft (int (limits.[0])) 3))) + " = ") + (unbox<string> (padLeft (int (bins.[0])) 2)))
        let mutable i: int = 1
        while i < n do
            printfn "%s" (((((">= " + (unbox<string> (padLeft (int (limits.[i - 1])) 3))) + " and < ") + (unbox<string> (padLeft (int (limits.[i])) 3))) + " = ") + (unbox<string> (padLeft (int (bins.[i])) 2)))
            i <- i + 1
        printfn "%s" (((">= " + (unbox<string> (padLeft (int (limits.[n - 1])) 3))) + "           = ") + (unbox<string> (padLeft (int (bins.[n])) 2)))
        printfn "%s" ""
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let limitsList: int array array = [|[|23; 37; 43; 53; 67; 83|]; [|14; 18; 249; 312; 389; 392; 513; 591; 634; 720|]|]
        let dataList: int array array = [|[|95; 21; 94; 12; 99; 4; 70; 75; 83; 93; 52; 80; 57; 5; 53; 86; 65; 17; 92; 83; 71; 61; 54; 58; 47; 16; 8; 9; 32; 84; 7; 87; 46; 19; 30; 37; 96; 6; 98; 40; 79; 97; 45; 64; 60; 29; 49; 36; 43; 55|]; [|445; 814; 519; 697; 700; 130; 255; 889; 481; 122; 932; 77; 323; 525; 570; 219; 367; 523; 442; 933; 416; 589; 930; 373; 202; 253; 775; 47; 731; 685; 293; 126; 133; 450; 545; 100; 741; 583; 763; 306; 655; 267; 248; 477; 549; 238; 62; 678; 98; 534; 622; 907; 406; 714; 184; 391; 913; 42; 560; 247; 346; 860; 56; 138; 546; 38; 985; 948; 58; 213; 799; 319; 390; 634; 458; 945; 733; 507; 916; 123; 345; 110; 720; 917; 313; 845; 426; 9; 457; 628; 410; 723; 354; 895; 881; 953; 677; 137; 397; 97; 854; 740; 83; 216; 421; 94; 517; 479; 292; 963; 376; 981; 480; 39; 257; 272; 157; 5; 316; 395; 787; 942; 456; 242; 759; 898; 576; 67; 298; 425; 894; 435; 831; 241; 989; 614; 987; 770; 384; 692; 698; 765; 331; 487; 251; 600; 879; 342; 982; 527; 736; 795; 585; 40; 54; 901; 408; 359; 577; 237; 605; 847; 353; 968; 832; 205; 838; 427; 876; 959; 686; 646; 835; 127; 621; 892; 443; 198; 988; 791; 466; 23; 707; 467; 33; 670; 921; 180; 991; 396; 160; 436; 717; 918; 8; 374; 101; 684; 727; 749|]|]
        let mutable i: int = 0
        while i < (int (Array.length limitsList)) do
            printfn "%s" (("Example " + (string (i + 1))) + "\n")
            let bins: int array = getBins (unbox<int array> (limitsList.[i])) (unbox<int array> (dataList.[i]))
            printBins (unbox<int array> (limitsList.[i])) bins
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
