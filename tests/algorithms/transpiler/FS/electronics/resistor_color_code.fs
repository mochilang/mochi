// Generated 2025-08-07 15:46 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let valid_colors: string array = [|"Black"; "Brown"; "Red"; "Orange"; "Yellow"; "Green"; "Blue"; "Violet"; "Grey"; "White"; "Gold"; "Silver"|]
let significant_figures_color_values: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("Black", 0); ("Brown", 1); ("Red", 2); ("Orange", 3); ("Yellow", 4); ("Green", 5); ("Blue", 6); ("Violet", 7); ("Grey", 8); ("White", 9)]
let multiplier_color_values: System.Collections.Generic.IDictionary<string, float> = _dictCreate [("Black", 1.0); ("Brown", 10.0); ("Red", 100.0); ("Orange", 1000.0); ("Yellow", 10000.0); ("Green", 100000.0); ("Blue", 1000000.0); ("Violet", 10000000.0); ("Grey", 100000000.0); ("White", 1000000000.0); ("Gold", 0.1); ("Silver", 0.01)]
let tolerance_color_values: System.Collections.Generic.IDictionary<string, float> = _dictCreate [("Brown", 1.0); ("Red", 2.0); ("Orange", 0.05); ("Yellow", 0.02); ("Green", 0.5); ("Blue", 0.25); ("Violet", 0.1); ("Grey", 0.01); ("Gold", 5.0); ("Silver", 10.0)]
let temperature_coeffecient_color_values: System.Collections.Generic.IDictionary<string, int> = _dictCreate [("Black", 250); ("Brown", 100); ("Red", 50); ("Orange", 15); ("Yellow", 25); ("Green", 20); ("Blue", 10); ("Violet", 5); ("Grey", 1)]
let rec contains (list: string array) (value: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list = list
    let mutable value = value
    try
        for c in Seq.map string (list) do
            if c = value then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_significant_digits (colors: string array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable colors = colors
    try
        let mutable digit: int = 0
        for color in Seq.map string (colors) do
            if not (significant_figures_color_values.ContainsKey(color)) then
                failwith (color + " is not a valid color for significant figure bands")
            digit <- (digit * 10) + (significant_figures_color_values.[(string (color))])
        __ret <- digit
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_multiplier (color: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable color = color
    try
        if not (multiplier_color_values.ContainsKey(color)) then
            failwith (color + " is not a valid color for multiplier band")
        __ret <- multiplier_color_values.[(string (color))]
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_tolerance (color: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable color = color
    try
        if not (tolerance_color_values.ContainsKey(color)) then
            failwith (color + " is not a valid color for tolerance band")
        __ret <- tolerance_color_values.[(string (color))]
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_temperature_coeffecient (color: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable color = color
    try
        if not (temperature_coeffecient_color_values.ContainsKey(color)) then
            failwith (color + " is not a valid color for temperature coeffecient band")
        __ret <- temperature_coeffecient_color_values.[(string (color))]
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_band_type_count (total: int) (typ: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable total = total
    let mutable typ = typ
    try
        if total = 3 then
            if typ = "significant" then
                __ret <- 2
                raise Return
            if typ = "multiplier" then
                __ret <- 1
                raise Return
            failwith (typ + " is not valid for a 3 band resistor")
        else
            if total = 4 then
                if typ = "significant" then
                    __ret <- 2
                    raise Return
                if typ = "multiplier" then
                    __ret <- 1
                    raise Return
                if typ = "tolerance" then
                    __ret <- 1
                    raise Return
                failwith (typ + " is not valid for a 4 band resistor")
            else
                if total = 5 then
                    if typ = "significant" then
                        __ret <- 3
                        raise Return
                    if typ = "multiplier" then
                        __ret <- 1
                        raise Return
                    if typ = "tolerance" then
                        __ret <- 1
                        raise Return
                    failwith (typ + " is not valid for a 5 band resistor")
                else
                    if total = 6 then
                        if typ = "significant" then
                            __ret <- 3
                            raise Return
                        if typ = "multiplier" then
                            __ret <- 1
                            raise Return
                        if typ = "tolerance" then
                            __ret <- 1
                            raise Return
                        if typ = "temp_coeffecient" then
                            __ret <- 1
                            raise Return
                        failwith (typ + " is not valid for a 6 band resistor")
                    else
                        failwith ((_str (total)) + " is not a valid number of bands")
        __ret
    with
        | Return -> __ret
let rec check_validity (number_of_bands: int) (colors: string array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number_of_bands = number_of_bands
    let mutable colors = colors
    try
        if (number_of_bands < 3) || (number_of_bands > 6) then
            failwith ("Invalid number of bands. Resistor bands must be 3 to 6")
        if number_of_bands <> (Seq.length (colors)) then
            failwith (((("Expecting " + (_str (number_of_bands))) + " colors, provided ") + (_str (Seq.length (colors)))) + " colors")
        for color in Seq.map string (colors) do
            if not (contains (valid_colors) (color)) then
                failwith (color + " is not a valid color")
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec calculate_resistance (number_of_bands: int) (color_code_list: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable number_of_bands = number_of_bands
    let mutable color_code_list = color_code_list
    try
        check_validity (number_of_bands) (color_code_list)
        let sig_count: int = get_band_type_count (number_of_bands) ("significant")
        let significant_colors: string array = Array.sub color_code_list 0 (sig_count - 0)
        let significant_digits: int = get_significant_digits (significant_colors)
        let multiplier_color: string = _idx color_code_list (sig_count)
        let multiplier: float = get_multiplier (multiplier_color)
        let mutable tolerance: float = 20.0
        if number_of_bands >= 4 then
            let tolerance_color: string = _idx color_code_list (sig_count + 1)
            tolerance <- get_tolerance (tolerance_color)
        let mutable temp_coeff: int = 0
        if number_of_bands = 6 then
            let temp_color: string = _idx color_code_list (sig_count + 2)
            temp_coeff <- get_temperature_coeffecient (temp_color)
        let resistance_value: float = multiplier * (float significant_digits)
        let mutable resistance_str: string = _str (resistance_value)
        if resistance_value = (float (int (resistance_value))) then
            resistance_str <- _str (int (resistance_value))
        let mutable answer: string = ((resistance_str + "Ω ±") + (_str (tolerance))) + "% "
        if temp_coeff <> 0 then
            answer <- (answer + (_str (temp_coeff))) + " ppm/K"
        __ret <- answer
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
