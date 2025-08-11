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
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
open System.Collections.Generic

let OPENWEATHERMAP_API_KEY: string = "demo"
let WEATHERSTACK_API_KEY: string = ""
let OPENWEATHERMAP_URL_BASE: string = "https://api.openweathermap.org/data/2.5/weather"
let WEATHERSTACK_URL_BASE: string = "http://api.weatherstack.com/current"
let rec http_get (url: string) (params: System.Collections.Generic.IDictionary<string, string>) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, string> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, string>>
    let mutable url = url
    let mutable params = params
    try
        __ret <- if params.ContainsKey("q") then (_dictCreate [("location", _dictGet params ((string ("q")))); ("temperature", "20")]) else (_dictCreate [("location", _dictGet params ((string ("query")))); ("temperature", "20")])
        raise Return
        __ret
    with
        | Return -> __ret
and current_weather (location: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, string>> array = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, string>> array>
    let mutable location = location
    try
        let mutable weather_data: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, string>> array = Array.empty<System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, string>>>
        if OPENWEATHERMAP_API_KEY <> "" then
            let params_openweathermap: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("q", location); ("appid", OPENWEATHERMAP_API_KEY)]
            let response_openweathermap: System.Collections.Generic.IDictionary<string, string> = http_get (OPENWEATHERMAP_URL_BASE) (params_openweathermap)
            weather_data <- Array.append weather_data [|(_dictCreate [("OpenWeatherMap", response_openweathermap)])|]
        if WEATHERSTACK_API_KEY <> "" then
            let params_weatherstack: System.Collections.Generic.IDictionary<string, string> = _dictCreate [("query", location); ("access_key", WEATHERSTACK_API_KEY)]
            let response_weatherstack: System.Collections.Generic.IDictionary<string, string> = http_get (WEATHERSTACK_URL_BASE) (params_weatherstack)
            weather_data <- Array.append weather_data [|(_dictCreate [("Weatherstack", response_weatherstack)])|]
        if (Seq.length (weather_data)) = 0 then
            failwith ("No API keys provided or no valid data returned.")
        __ret <- weather_data
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let data: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, string>> array = current_weather ("New York")
        printfn "%s" (_str (data))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
