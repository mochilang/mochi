// Generated 2025-08-07 14:57 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_symmetric_tree () =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    try
        __ret <- [|[|1; 1; 2|]; [|2; 3; 4|]; [|2; 5; 6|]; [|3; -1; -1|]; [|4; -1; -1|]; [|4; -1; -1|]; [|3; -1; -1|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_asymmetric_tree () =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    try
        __ret <- [|[|1; 1; 2|]; [|2; 3; 4|]; [|2; 5; 6|]; [|3; -1; -1|]; [|4; -1; -1|]; [|3; -1; -1|]; [|4; -1; -1|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_symmetric_tree (tree: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable tree = tree
    try
        let mutable stack: int array = [|_idx (_idx tree (0)) (1); _idx (_idx tree (0)) (2)|]
        try
            while (Seq.length (stack)) >= 2 do
                try
                    let left: int = _idx stack ((Seq.length (stack)) - 2)
                    let right: int = _idx stack ((Seq.length (stack)) - 1)
                    stack <- Array.sub stack 0 (((Seq.length (stack)) - 2) - 0)
                    if (left = (-1)) && (right = (-1)) then
                        raise Continue
                    if (left = (-1)) || (right = (-1)) then
                        __ret <- false
                        raise Return
                    let lnode: int array = _idx tree (left)
                    let rnode: int array = _idx tree (right)
                    if (_idx lnode (0)) <> (_idx rnode (0)) then
                        __ret <- false
                        raise Return
                    stack <- Array.append stack [|_idx lnode (1)|]
                    stack <- Array.append stack [|_idx rnode (2)|]
                    stack <- Array.append stack [|_idx lnode (2)|]
                    stack <- Array.append stack [|_idx rnode (1)|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let symmetric_tree: int array array = make_symmetric_tree()
let asymmetric_tree: int array array = make_asymmetric_tree()
printfn "%s" (_str (is_symmetric_tree (symmetric_tree)))
printfn "%s" (_str (is_symmetric_tree (asymmetric_tree)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
