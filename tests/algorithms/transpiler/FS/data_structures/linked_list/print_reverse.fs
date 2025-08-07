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
let rec empty_list () =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    try
        __ret <- { data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and append_value (list: LinkedList) (value: int) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable list = list
    let mutable value = value
    try
        let mutable d: int array = list.data
        d <- Array.append d [|value|]
        __ret <- { data = d }
        raise Return
        __ret
    with
        | Return -> __ret
and extend_list (list: LinkedList) (items: int array) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable list = list
    let mutable items = items
    try
        let mutable result: LinkedList = list
        let mutable i: int = 0
        while i < (Seq.length (items)) do
            result <- append_value (result) (_idx items (i))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (list: LinkedList) =
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
and make_linked_list (items: int array) =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    let mutable items = items
    try
        if (Seq.length (items)) = 0 then
            failwith ("The Elements List is empty")
        let mutable ll: LinkedList = empty_list()
        ll <- extend_list (ll) (items)
        __ret <- ll
        raise Return
        __ret
    with
        | Return -> __ret
and in_reverse (list: LinkedList) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable list = list
    try
        if (Seq.length (list.data)) = 0 then
            __ret <- ""
            raise Return
        let mutable i: int = (Seq.length (list.data)) - 1
        let mutable s: string = _str (_idx (list.data) (i))
        i <- i - 1
        while i >= 0 do
            s <- (s + " <- ") + (_str (_idx (list.data) (i)))
            i <- i - 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let linked_list: LinkedList = make_linked_list (unbox<int array> [|14; 52; 14; 12; 43|])
        printfn "%s" ("Linked List:  " + (to_string (linked_list)))
        printfn "%s" ("Reverse List: " + (in_reverse (linked_list)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
