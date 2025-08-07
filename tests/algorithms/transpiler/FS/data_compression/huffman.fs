// Generated 2025-08-07 10:31 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type Huffman =
    | Leaf of string * int
    | Node of int * Huffman * Huffman
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec get_freq (n: Huffman) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- int ((match n with
            | Leaf(_, f) -> f
            | Node(f, _, _) -> f))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_nodes (nodes: Huffman array) =
    let mutable __ret : Huffman array = Unchecked.defaultof<Huffman array>
    let mutable nodes = nodes
    try
        let mutable arr: Huffman array = nodes
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            let key: Huffman = _idx arr (i)
            let mutable j: int = i - 1
            while (j >= 0) && ((get_freq (_idx arr (j))) > (get_freq (key))) do
                arr.[j + 1] <- _idx arr (j)
                j <- j - 1
            arr.[j + 1] <- key
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec rest (nodes: Huffman array) =
    let mutable __ret : Huffman array = Unchecked.defaultof<Huffman array>
    let mutable nodes = nodes
    try
        let mutable res: Huffman array = [||]
        let mutable i: int = 1
        while i < (Seq.length (nodes)) do
            res <- Array.append res [|_idx nodes (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_freq (text: string) =
    let mutable __ret : Huffman array = Unchecked.defaultof<Huffman array>
    let mutable text = text
    try
        let mutable chars: string array = [||]
        let mutable freqs: int array = [||]
        let mutable i: int = 0
        try
            while i < (String.length (text)) do
                try
                    let c: string = _substring text i (i + 1)
                    let mutable j: int = 0
                    let mutable found: bool = false
                    try
                        while j < (Seq.length (chars)) do
                            try
                                if (_idx chars (j)) = c then
                                    freqs.[j] <- (_idx freqs (j)) + 1
                                    found <- true
                                    raise Break
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    if not found then
                        chars <- Array.append chars [|c|]
                        freqs <- Array.append freqs [|1|]
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let mutable leaves: Huffman array = [||]
        let mutable k: int = 0
        while k < (Seq.length (chars)) do
            leaves <- Array.append leaves [|unbox<Huffman> (Leaf(_idx chars (k), _idx freqs (k)))|]
            k <- k + 1
        __ret <- sort_nodes (leaves)
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_tree (nodes: Huffman array) =
    let mutable __ret : Huffman = Unchecked.defaultof<Huffman>
    let mutable nodes = nodes
    try
        let mutable arr: Huffman array = nodes
        while (Seq.length (arr)) > 1 do
            let left: Huffman = _idx arr (0)
            arr <- rest (arr)
            let right: Huffman = _idx arr (0)
            arr <- rest (arr)
            let node: Huffman = Node((get_freq (left)) + (get_freq (right)), left, right)
            arr <- Array.append arr [|node|]
            arr <- sort_nodes (arr)
        __ret <- _idx arr (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec concat_pairs (a: string array array) (b: string array array) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: string array array = a
        let mutable i: int = 0
        while i < (Seq.length (b)) do
            res <- Array.append res [|_idx b (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec collect_codes (tree: Huffman) (prefix: string) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable tree = tree
    let mutable prefix = prefix
    try
        __ret <- unbox<string array array> ((match tree with
            | Leaf(s, _) -> [|[|s; prefix|]|]
            | Node(_, l, r) -> concat_pairs (collect_codes (l) (prefix + "0")) (collect_codes (r) (prefix + "1"))))
        raise Return
        __ret
    with
        | Return -> __ret
let rec find_code (pairs: string array array) (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable pairs = pairs
    let mutable ch = ch
    try
        let mutable i: int = 0
        while i < (Seq.length (pairs)) do
            if (_idx (_idx pairs (i)) (0)) = ch then
                __ret <- _idx (_idx pairs (i)) (1)
                raise Return
            i <- i + 1
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
let rec huffman_encode (text: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable text = text
    try
        if text = "" then
            __ret <- ""
            raise Return
        let mutable leaves: Huffman array = count_freq (text)
        let tree: Huffman = build_tree (leaves)
        let codes: string array array = collect_codes (tree) ("")
        let mutable encoded: string = ""
        let mutable i: int = 0
        while i < (String.length (text)) do
            let c: string = _substring text i (i + 1)
            encoded <- (encoded + (find_code (codes) (c))) + " "
            i <- i + 1
        __ret <- encoded
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (huffman_encode ("beep boop beer!"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
