// Generated 2025-08-11 15:32 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type TransformTables = {
    mutable _costs: int array array
    mutable _ops: string array array
}
let rec string_to_chars (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable chars: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (String.length (s)) do
            chars <- Array.append chars [|(_substring s i (i + 1))|]
            i <- i + 1
        __ret <- chars
        raise Return
        __ret
    with
        | Return -> __ret
and join_chars (chars: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable chars = chars
    try
        let mutable res: string = ""
        let mutable i: int = 0
        while i < (Seq.length (chars)) do
            res <- res + (_idx chars (int i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and insert_at (chars: string array) (index: int) (ch: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable chars = chars
    let mutable index = index
    let mutable ch = ch
    try
        let mutable res: string array = Array.empty<string>
        let mutable i: int = 0
        while i < index do
            res <- Array.append res [|(_idx chars (int i))|]
            i <- i + 1
        res <- Array.append res [|ch|]
        while i < (Seq.length (chars)) do
            res <- Array.append res [|(_idx chars (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and remove_at (chars: string array) (index: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable chars = chars
    let mutable index = index
    try
        let mutable res: string array = Array.empty<string>
        let mutable i: int = 0
        while i < (Seq.length (chars)) do
            if i <> index then
                res <- Array.append res [|(_idx chars (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and make_matrix_int (rows: int) (cols: int) (init: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable rows = rows
    let mutable cols = cols
    let mutable init = init
    try
        let mutable matrix: int array array = Array.empty<int array>
        for _ in 0 .. (rows - 1) do
            let mutable row: int array = Array.empty<int>
            for _2 in 0 .. (cols - 1) do
                row <- Array.append row [|init|]
            matrix <- Array.append matrix [|row|]
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
and make_matrix_string (rows: int) (cols: int) (init: string) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable rows = rows
    let mutable cols = cols
    let mutable init = init
    try
        let mutable matrix: string array array = Array.empty<string array>
        for _ in 0 .. (rows - 1) do
            let mutable row: string array = Array.empty<string>
            for _2 in 0 .. (cols - 1) do
                row <- Array.append row [|init|]
            matrix <- Array.append matrix [|row|]
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
and compute_transform_tables (source_string: string) (destination_string: string) (copy_cost: int) (replace_cost: int) (delete_cost: int) (insert_cost: int) =
    let mutable __ret : TransformTables = Unchecked.defaultof<TransformTables>
    let mutable source_string = source_string
    let mutable destination_string = destination_string
    let mutable copy_cost = copy_cost
    let mutable replace_cost = replace_cost
    let mutable delete_cost = delete_cost
    let mutable insert_cost = insert_cost
    try
        let source_seq: string array = string_to_chars (source_string)
        let dest_seq: string array = string_to_chars (destination_string)
        let m: int = Seq.length (source_seq)
        let n: int = Seq.length (dest_seq)
        let mutable _costs: int array array = make_matrix_int (m + 1) (n + 1) (0)
        let mutable _ops: string array array = make_matrix_string (m + 1) (n + 1) ("0")
        let mutable i: int = 1
        while i <= m do
            _costs.[int i].[int 0] <- int ((int64 i) * (int64 delete_cost))
            _ops.[int i].[int 0] <- "D" + (_idx source_seq (int (i - 1)))
            i <- i + 1
        let mutable j: int = 1
        while j <= n do
            _costs.[int 0].[int j] <- int ((int64 j) * (int64 insert_cost))
            _ops.[int 0].[int j] <- "I" + (_idx dest_seq (int (j - 1)))
            j <- j + 1
        i <- 1
        while i <= m do
            j <- 1
            while j <= n do
                if (_idx source_seq (int (i - 1))) = (_idx dest_seq (int (j - 1))) then
                    _costs.[int i].[int j] <- (_idx (_idx _costs (int (i - 1))) (int (j - 1))) + copy_cost
                    _ops.[int i].[int j] <- "C" + (_idx source_seq (int (i - 1)))
                else
                    _costs.[int i].[int j] <- (_idx (_idx _costs (int (i - 1))) (int (j - 1))) + replace_cost
                    _ops.[int i].[int j] <- ("R" + (_idx source_seq (int (i - 1)))) + (_idx dest_seq (int (j - 1)))
                if ((_idx (_idx _costs (int (i - 1))) (int j)) + delete_cost) < (_idx (_idx _costs (int i)) (int j)) then
                    _costs.[int i].[int j] <- (_idx (_idx _costs (int (i - 1))) (int j)) + delete_cost
                    _ops.[int i].[int j] <- "D" + (_idx source_seq (int (i - 1)))
                if ((_idx (_idx _costs (int i)) (int (j - 1))) + insert_cost) < (_idx (_idx _costs (int i)) (int j)) then
                    _costs.[int i].[int j] <- (_idx (_idx _costs (int i)) (int (j - 1))) + insert_cost
                    _ops.[int i].[int j] <- "I" + (_idx dest_seq (int (j - 1)))
                j <- j + 1
            i <- i + 1
        __ret <- { _costs = _costs; _ops = _ops }
        raise Return
        __ret
    with
        | Return -> __ret
and assemble_transformation (_ops: string array array) (i: int) (j: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable _ops = _ops
    let mutable i = i
    let mutable j = j
    try
        if (i = 0) && (j = 0) then
            __ret <- Array.empty<string>
            raise Return
        let op: string = _idx (_idx _ops (int i)) (int j)
        let kind: string = _substring op 0 1
        if (kind = "C") || (kind = "R") then
            let mutable seq: string array = assemble_transformation (_ops) (i - 1) (j - 1)
            seq <- Array.append seq [|op|]
            __ret <- seq
            raise Return
        else
            if kind = "D" then
                let mutable seq: string array = assemble_transformation (_ops) (i - 1) (j)
                seq <- Array.append seq [|op|]
                __ret <- seq
                raise Return
            else
                let mutable seq: string array = assemble_transformation (_ops) (i) (j - 1)
                seq <- Array.append seq [|op|]
                __ret <- seq
                raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let copy_cost: int = -1
        let replace_cost: int = 1
        let delete_cost: int = 2
        let insert_cost: int = 2
        let src: string = "Python"
        let dst: string = "Algorithms"
        let tables: TransformTables = compute_transform_tables (src) (dst) (copy_cost) (replace_cost) (delete_cost) (insert_cost)
        let operations: string array array = tables._ops
        let m: int = Seq.length (operations)
        let n: int = Seq.length (_idx operations (int 0))
        let mutable sequence: string array = assemble_transformation (operations) (m - 1) (n - 1)
        let mutable string_list: string array = string_to_chars (src)
        let mutable idx: int = 0
        let mutable cost: int = 0
        let mutable k: int = 0
        while k < (Seq.length (sequence)) do
            printfn "%s" (join_chars (string_list))
            let op: string = _idx sequence (int k)
            let kind: string = _substring op 0 1
            if kind = "C" then
                cost <- cost + copy_cost
            else
                if kind = "R" then
                    string_list.[int idx] <- _substring op 2 3
                    cost <- cost + replace_cost
                else
                    if kind = "D" then
                        string_list <- remove_at (string_list) (idx)
                        cost <- cost + delete_cost
                    else
                        string_list <- insert_at (string_list) (idx) (_substring op 1 2)
                        cost <- cost + insert_cost
            idx <- idx + 1
            k <- k + 1
        printfn "%s" (join_chars (string_list))
        printfn "%s" ("Cost: " + (_str (cost)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
