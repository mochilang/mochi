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
type SinglyLinkedList = {
    data: int array
}
type DeleteResult = {
    list: SinglyLinkedList
    value: int
}
let rec empty_list () =
    let mutable __ret : SinglyLinkedList = Unchecked.defaultof<SinglyLinkedList>
    try
        __ret <- { data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and length (list: SinglyLinkedList) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable list = list
    try
        __ret <- Seq.length (list.data)
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (list: SinglyLinkedList) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list = list
    try
        __ret <- (Seq.length (list.data)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (list: SinglyLinkedList) =
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
and insert_nth (list: SinglyLinkedList) (index: int) (value: int) =
    let mutable __ret : SinglyLinkedList = Unchecked.defaultof<SinglyLinkedList>
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
and insert_head (list: SinglyLinkedList) (value: int) =
    let mutable __ret : SinglyLinkedList = Unchecked.defaultof<SinglyLinkedList>
    let mutable list = list
    let mutable value = value
    try
        __ret <- insert_nth (list) (0) (value)
        raise Return
        __ret
    with
        | Return -> __ret
and insert_tail (list: SinglyLinkedList) (value: int) =
    let mutable __ret : SinglyLinkedList = Unchecked.defaultof<SinglyLinkedList>
    let mutable list = list
    let mutable value = value
    try
        __ret <- insert_nth (list) (Seq.length (list.data)) (value)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_nth (list: SinglyLinkedList) (index: int) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    let mutable index = index
    try
        if (index < 0) || (index >= (Seq.length (list.data))) then
            failwith ("index out of range")
        let mutable res: int array = [||]
        let mutable ``val``: int = 0
        let mutable i: int = 0
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
and delete_head (list: SinglyLinkedList) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    try
        __ret <- delete_nth (list) (0)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_tail (list: SinglyLinkedList) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    try
        __ret <- delete_nth (list) ((Seq.length (list.data)) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and get_item (list: SinglyLinkedList) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable list = list
    let mutable index = index
    try
        if (index < 0) || (index >= (Seq.length (list.data))) then
            failwith ("index out of range")
        __ret <- _idx (list.data) (index)
        raise Return
        __ret
    with
        | Return -> __ret
and set_item (list: SinglyLinkedList) (index: int) (value: int) =
    let mutable __ret : SinglyLinkedList = Unchecked.defaultof<SinglyLinkedList>
    let mutable list = list
    let mutable index = index
    let mutable value = value
    try
        if (index < 0) || (index >= (Seq.length (list.data))) then
            failwith ("index out of range")
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (list.data)) do
            if i = index then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|_idx (list.data) (i)|]
            i <- i + 1
        __ret <- { data = res }
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_list (list: SinglyLinkedList) =
    let mutable __ret : SinglyLinkedList = Unchecked.defaultof<SinglyLinkedList>
    let mutable list = list
    try
        let mutable res: int array = [||]
        let mutable i: int = (Seq.length (list.data)) - 1
        while i >= 0 do
            res <- Array.append res [|_idx (list.data) (i)|]
            i <- i - 1
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
        let mutable lst: SinglyLinkedList = empty_list()
        let mutable i: int = 1
        while i <= 5 do
            lst <- insert_tail (lst) (i)
            i <- i + 1
        printfn "%s" (to_string (lst))
        lst <- insert_head (lst) (0)
        printfn "%s" (to_string (lst))
        let mutable del: DeleteResult = delete_head (lst)
        lst <- del.list
        printfn "%s" (_str (del.value))
        del <- delete_tail (lst)
        lst <- del.list
        printfn "%s" (_str (del.value))
        del <- delete_nth (lst) (2)
        lst <- del.list
        printfn "%s" (_str (del.value))
        lst <- set_item (lst) (1) (99)
        printfn "%s" (_str (get_item (lst) (1)))
        lst <- reverse_list (lst)
        printfn "%s" (to_string (lst))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
