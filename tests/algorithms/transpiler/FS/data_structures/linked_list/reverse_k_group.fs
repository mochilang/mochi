// Generated 2025-08-07 14:57 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type LinkedList = {
    data: int array
}
let rec to_string (list: LinkedList) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable list = list
    try
        if (Seq.length (list.data)) = 0 then
            __ret <- ""
            raise Return
        let mutable s: string = _str (_idx (list.data) (0))
        let mutable i: int = 1
        while i < (Seq.length (list.data)) do
            s <- (s + " -> ") + (_str (_idx (list.data) (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_k_nodes (list: LinkedList) (k: int) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable list = list
    let mutable k = k
    try
        if k <= 1 then
            __ret <- list
            raise Return
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (list.data)) do
            let mutable j: int = 0
            let mutable group: int array = [||]
            while (j < k) && ((i + j) < (Seq.length (list.data))) do
                group <- Array.append group [|_idx (list.data) (i + j)|]
                j <- j + 1
            if (Seq.length (group)) = k then
                let mutable g: int = k - 1
                while g >= 0 do
                    res <- Array.append res [|_idx group (g)|]
                    g <- g - 1
            else
                let mutable g: int = 0
                while g < (Seq.length (group)) do
                    res <- Array.append res [|_idx group (g)|]
                    g <- g + 1
            i <- i + k
        __ret <- { data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable ll: LinkedList = { data = [|1; 2; 3; 4; 5|] }
        printfn "%s" ("Original Linked List: " + (to_string (ll)))
        let mutable k: int = 2
        ll <- reverse_k_nodes (ll) (k)
        printfn "%s" ((("After reversing groups of size " + (_str (k))) + ": ") + (to_string (ll)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
