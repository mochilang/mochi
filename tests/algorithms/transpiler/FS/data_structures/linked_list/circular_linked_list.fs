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
type CircularLinkedList = {
    data: int array
}
type DeleteResult = {
    list: CircularLinkedList
    value: int
}
let rec empty_list () =
    let mutable __ret : CircularLinkedList = Unchecked.defaultof<CircularLinkedList>
    try
        __ret <- { data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and length (list: CircularLinkedList) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable list = list
    try
        __ret <- Seq.length (list.data)
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (list: CircularLinkedList) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list = list
    try
        __ret <- (Seq.length (list.data)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (list: CircularLinkedList) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable list = list
    try
        if (Seq.length (list.data)) = 0 then
            __ret <- ""
            raise Return
        let mutable s: string = _str (_idx (list.data) (0))
        let mutable i: int = 1
        while i < (Seq.length (list.data)) do
            s <- (s + "->") + (_str (_idx (list.data) (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and insert_nth (list: CircularLinkedList) (index: int) (value: int) =
    let mutable __ret : CircularLinkedList = Unchecked.defaultof<CircularLinkedList>
    let mutable list = list
    let mutable index = index
    let mutable value = value
    try
        if (index < 0) || (index > (Seq.length (list.data))) then
            failwith ("index out of range")
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < index do
            res <- Array.append res [|_idx (list.data) (i)|]
            i <- i + 1
        res <- Array.append res [|value|]
        while i < (Seq.length (list.data)) do
            res <- Array.append res [|_idx (list.data) (i)|]
            i <- i + 1
        __ret <- { data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and insert_head (list: CircularLinkedList) (value: int) =
    let mutable __ret : CircularLinkedList = Unchecked.defaultof<CircularLinkedList>
    let mutable list = list
    let mutable value = value
    try
        __ret <- insert_nth (list) (0) (value)
        raise Return
        __ret
    with
        | Return -> __ret
and insert_tail (list: CircularLinkedList) (value: int) =
    let mutable __ret : CircularLinkedList = Unchecked.defaultof<CircularLinkedList>
    let mutable list = list
    let mutable value = value
    try
        __ret <- insert_nth (list) (Seq.length (list.data)) (value)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_nth (list: CircularLinkedList) (index: int) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    let mutable index = index
    try
        if (index < 0) || (index >= (Seq.length (list.data))) then
            failwith ("index out of range")
        let mutable res: int array = [||]
        let mutable i: int = 0
        let mutable ``val``: int = 0
        while i < (Seq.length (list.data)) do
            if i = index then
                ``val`` <- _idx (list.data) (i)
            else
                res <- Array.append res [|_idx (list.data) (i)|]
            i <- i + 1
        __ret <- { list = { data = res }; value = ``val`` }
        raise Return
        __ret
    with
        | Return -> __ret
and delete_front (list: CircularLinkedList) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    try
        __ret <- delete_nth (list) (0)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_tail (list: CircularLinkedList) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    try
        __ret <- delete_nth (list) ((Seq.length (list.data)) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable cll: CircularLinkedList = empty_list()
        let mutable i: int = 0
        while i < 5 do
            cll <- insert_tail (cll) (i + 1)
            i <- i + 1
        printfn "%s" (to_string (cll))
        cll <- insert_tail (cll) (6)
        printfn "%s" (to_string (cll))
        cll <- insert_head (cll) (0)
        printfn "%s" (to_string (cll))
        let mutable res: DeleteResult = delete_front (cll)
        cll <- res.list
        printfn "%d" (res.value)
        res <- delete_tail (cll)
        cll <- res.list
        printfn "%d" (res.value)
        res <- delete_nth (cll) (2)
        cll <- res.list
        printfn "%d" (res.value)
        printfn "%s" (to_string (cll))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
