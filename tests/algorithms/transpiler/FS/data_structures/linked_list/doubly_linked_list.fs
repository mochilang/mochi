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
type DoublyLinkedList = {
    data: int array
}
type DeleteResult = {
    list: DoublyLinkedList
    value: int
}
let rec empty_list () =
    let mutable __ret : DoublyLinkedList = Unchecked.defaultof<DoublyLinkedList>
    try
        __ret <- { data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and length (list: DoublyLinkedList) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable list = list
    try
        __ret <- Seq.length (list.data)
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (list: DoublyLinkedList) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list = list
    try
        __ret <- (Seq.length (list.data)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (list: DoublyLinkedList) =
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
and insert_nth (list: DoublyLinkedList) (index: int) (value: int) =
    let mutable __ret : DoublyLinkedList = Unchecked.defaultof<DoublyLinkedList>
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
and insert_head (list: DoublyLinkedList) (value: int) =
    let mutable __ret : DoublyLinkedList = Unchecked.defaultof<DoublyLinkedList>
    let mutable list = list
    let mutable value = value
    try
        __ret <- insert_nth (list) (0) (value)
        raise Return
        __ret
    with
        | Return -> __ret
and insert_tail (list: DoublyLinkedList) (value: int) =
    let mutable __ret : DoublyLinkedList = Unchecked.defaultof<DoublyLinkedList>
    let mutable list = list
    let mutable value = value
    try
        __ret <- insert_nth (list) (Seq.length (list.data)) (value)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_nth (list: DoublyLinkedList) (index: int) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    let mutable index = index
    try
        if (index < 0) || (index >= (Seq.length (list.data))) then
            failwith ("index out of range")
        let mutable res: int array = [||]
        let mutable i: int = 0
        let mutable removed: int = 0
        while i < (Seq.length (list.data)) do
            if i = index then
                removed <- _idx (list.data) (i)
            else
                res <- Array.append res [|_idx (list.data) (i)|]
            i <- i + 1
        __ret <- { list = { data = res }; value = removed }
        raise Return
        __ret
    with
        | Return -> __ret
and delete_head (list: DoublyLinkedList) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    try
        __ret <- delete_nth (list) (0)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_tail (list: DoublyLinkedList) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    try
        __ret <- delete_nth (list) ((Seq.length (list.data)) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
and delete_value (list: DoublyLinkedList) (value: int) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable list = list
    let mutable value = value
    try
        let mutable idx: int = 0
        let mutable found: bool = false
        try
            while idx < (Seq.length (list.data)) do
                try
                    if (_idx (list.data) (idx)) = value then
                        found <- true
                        raise Break
                    idx <- idx + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        if not found then
            failwith ("value not found")
        __ret <- delete_nth (list) (idx)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable dll: DoublyLinkedList = empty_list()
        dll <- insert_tail (dll) (1)
        dll <- insert_tail (dll) (2)
        dll <- insert_tail (dll) (3)
        printfn "%s" (to_string (dll))
        dll <- insert_head (dll) (0)
        printfn "%s" (to_string (dll))
        dll <- insert_nth (dll) (2) (9)
        printfn "%s" (to_string (dll))
        let mutable res: DeleteResult = delete_nth (dll) (2)
        dll <- res.list
        printfn "%d" (res.value)
        printfn "%s" (to_string (dll))
        res <- delete_tail (dll)
        dll <- res.list
        printfn "%d" (res.value)
        printfn "%s" (to_string (dll))
        res <- delete_value (dll) (1)
        dll <- res.list
        printfn "%d" (res.value)
        printfn "%s" (to_string (dll))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
