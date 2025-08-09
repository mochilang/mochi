// Generated 2025-08-09 10:14 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
type Thing = {
    mutable _name: string
    mutable _value: float
    mutable _weight: float
}
type GreedyResult = {
    mutable _items: Thing array
    mutable _total_value: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec get_value (t: Thing) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable t = t
    try
        __ret <- t._value
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_weight (t: Thing) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable t = t
    try
        __ret <- t._weight
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_name (t: Thing) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable t = t
    try
        __ret <- t._name
        raise Return
        __ret
    with
        | Return -> __ret
let rec value_weight (t: Thing) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable t = t
    try
        __ret <- (t._value) / (t._weight)
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_menu (names: string array) (values: float array) (weights: float array) =
    let mutable __ret : Thing array = Unchecked.defaultof<Thing array>
    let mutable names = names
    let mutable values = values
    let mutable weights = weights
    try
        let mutable menu: Thing array = Array.empty<Thing>
        let mutable i: int = 0
        while ((i < (Seq.length (values))) && (i < (Seq.length (names)))) && (i < (Seq.length (weights))) do
            menu <- Array.append menu [|{ _name = _idx names (int i); _value = _idx values (int i); _weight = _idx weights (int i) }|]
            i <- i + 1
        __ret <- menu
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_desc (_items: Thing array) (key_func: Thing -> float) =
    let mutable __ret : Thing array = Unchecked.defaultof<Thing array>
    let mutable _items = _items
    let mutable key_func = key_func
    try
        let mutable arr: Thing array = Array.empty<Thing>
        let mutable i: int = 0
        while i < (Seq.length (_items)) do
            arr <- Array.append arr [|(_idx _items (int i))|]
            i <- i + 1
        let mutable j: int = 1
        while j < (Seq.length (arr)) do
            let key_item: Thing = _idx arr (int j)
            let key_val: float = key_func (key_item)
            let mutable k: int = j - 1
            while (k >= 0) && ((float (key_func (_idx arr (int k)))) < key_val) do
                arr.[int (k + 1)] <- _idx arr (int k)
                k <- k - 1
            arr.[int (k + 1)] <- key_item
            j <- j + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec greedy (_items: Thing array) (max_cost: float) (key_func: Thing -> float) =
    let mutable __ret : GreedyResult = Unchecked.defaultof<GreedyResult>
    let mutable _items = _items
    let mutable max_cost = max_cost
    let mutable key_func = key_func
    try
        let items_copy: Thing array = sort_desc (_items) (key_func)
        let mutable result: Thing array = Array.empty<Thing>
        let mutable _total_value: float = 0.0
        let mutable total_cost: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (items_copy)) do
            let it: Thing = _idx items_copy (int i)
            let w: float = get_weight (it)
            if (total_cost + w) <= max_cost then
                result <- Array.append result [|it|]
                total_cost <- total_cost + w
                _total_value <- _total_value + (get_value (it))
            i <- i + 1
        __ret <- { _items = result; _total_value = _total_value }
        raise Return
        __ret
    with
        | Return -> __ret
let rec thing_to_string (t: Thing) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable t = t
    try
        __ret <- ((((("Thing(" + (t._name)) + ", ") + (_str (t._value))) + ", ") + (_str (t._weight))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let rec list_to_string (ts: Thing array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ts = ts
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length (ts)) do
            s <- s + (thing_to_string (_idx ts (int i)))
            if i < ((Seq.length (ts)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let food: string array = unbox<string array> [|"Burger"; "Pizza"; "Coca Cola"; "Rice"; "Sambhar"; "Chicken"; "Fries"; "Milk"|]
let _value: float array = unbox<float array> [|80.0; 100.0; 60.0; 70.0; 50.0; 110.0; 90.0; 60.0|]
let _weight: float array = unbox<float array> [|40.0; 60.0; 40.0; 70.0; 100.0; 85.0; 55.0; 70.0|]
let foods: Thing array = build_menu (food) (_value) (_weight)
printfn "%s" (list_to_string (foods))
let res: GreedyResult = greedy (foods) (500.0) (unbox<Thing -> float> get_value)
printfn "%s" (list_to_string (res._items))
printfn "%s" (_str (res._total_value))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
