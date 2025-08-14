// Generated 2025-08-14 16:28 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type GasStation = {
    mutable _gas_quantity: int
    mutable _cost: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec get_gas_stations (gas_quantities: int array) (costs: int array) =
    let mutable __ret : GasStation array = Unchecked.defaultof<GasStation array>
    let mutable gas_quantities = gas_quantities
    let mutable costs = costs
    try
        let mutable stations: GasStation array = Array.empty<GasStation>
        let mutable i: int = 0
        while i < (Seq.length (gas_quantities)) do
            stations <- Array.append stations [|{ _gas_quantity = _idx gas_quantities (int i); _cost = _idx costs (int i) }|]
            i <- i + 1
        __ret <- stations
        raise Return
        __ret
    with
        | Return -> __ret
and can_complete_journey (gas_stations: GasStation array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable gas_stations = gas_stations
    try
        let mutable total_gas: int = 0
        let mutable total_cost: int = 0
        let mutable i: int = 0
        while i < (Seq.length (gas_stations)) do
            total_gas <- int (total_gas + (int ((_idx gas_stations (int i))._gas_quantity)))
            total_cost <- int (total_cost + (int ((_idx gas_stations (int i))._cost)))
            i <- i + 1
        if total_gas < total_cost then
            __ret <- -1
            raise Return
        let mutable start: int = 0
        let mutable net: int = 0
        i <- 0
        while i < (Seq.length (gas_stations)) do
            let station: GasStation = _idx gas_stations (int i)
            net <- int ((float (net + (int (station._gas_quantity)))) - (float (station._cost)))
            if net < 0 then
                start <- i + 1
                net <- 0
            i <- i + 1
        __ret <- start
        raise Return
        __ret
    with
        | Return -> __ret
let example1: GasStation array = get_gas_stations (unbox<int array> [|1; 2; 3; 4; 5|]) (unbox<int array> [|3; 4; 5; 1; 2|])
ignore (printfn "%s" (_str (can_complete_journey (example1))))
let example2: GasStation array = get_gas_stations (unbox<int array> [|2; 3; 4|]) (unbox<int array> [|3; 4; 3|])
ignore (printfn "%s" (_str (can_complete_journey (example2))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
