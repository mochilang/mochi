// Generated 2025-08-07 12:04 +0700

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
type Node = {
    key: int
    left: int
    right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec inorder (nodes: Node array) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nodes = nodes
    let mutable idx = idx
    try
        if idx = (-1) then
            __ret <- Array.empty<int>
            raise Return
        let node: Node = _idx nodes (idx)
        let mutable result: int array = inorder (nodes) (node.left)
        result <- Array.append result [|node.key|]
        result <- unbox<int array> (Array.append (result) (inorder (nodes) (node.right)))
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec floor_ceiling (nodes: Node array) (idx: int) (key: int) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable nodes = nodes
    let mutable idx = idx
    let mutable key = key
    try
        let mutable floor_val: obj = box (null)
        let mutable ceiling_val: obj = box (null)
        let mutable current: int = idx
        try
            while current <> (-1) do
                try
                    let node: Node = _idx nodes (current)
                    if (node.key) = key then
                        floor_val <- box (node.key)
                        ceiling_val <- box (node.key)
                        raise Break
                    if key < (node.key) then
                        ceiling_val <- box (node.key)
                        current <- node.left
                    else
                        floor_val <- box (node.key)
                        current <- node.right
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- [|floor_val; ceiling_val|]
        raise Return
        __ret
    with
        | Return -> __ret
let tree: Node array = [|{ key = 10; left = 1; right = 2 }; { key = 5; left = 3; right = 4 }; { key = 20; left = 5; right = 6 }; { key = 3; left = -1; right = -1 }; { key = 7; left = -1; right = -1 }; { key = 15; left = -1; right = -1 }; { key = 25; left = -1; right = -1 }|]
printfn "%s" (_str (inorder (tree) (0)))
printfn "%s" (_str (floor_ceiling (tree) (0) (8)))
printfn "%s" (_str (floor_ceiling (tree) (0) (14)))
printfn "%s" (_str (floor_ceiling (tree) (0) (-1)))
printfn "%s" (_str (floor_ceiling (tree) (0) (30)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
