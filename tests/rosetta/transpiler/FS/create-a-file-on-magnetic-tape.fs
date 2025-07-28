// Generated 2025-07-28 11:14 +0700

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
open System

let rec gzipWriter (w: obj) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable w = w
    try
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and tarWriter (w: obj) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable w = w
    try
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and tarWriteHeader (w: obj) (hdr: Map<string, obj>) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable w = w
    let mutable hdr = hdr
    try

        __ret
    with
        | Return -> __ret
and tarWrite (w: obj) (data: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable w = w
    let mutable data = data
    try

        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let filename: string = "TAPE.FILE"
        let data: string = ""
        let outfile: string = ""
        let gzipFlag: bool = false
        let mutable w: obj = Unchecked.defaultof<obj>
        if outfile <> "" then
            w <- null
        if gzipFlag then
            w <- gzipWriter w
        w <- tarWriter w
        let mutable hdr: Map<string, obj> = Map.ofList [("Name", box filename); ("Mode", box 432); ("Size", box (String.length data)); ("ModTime", box (_now())); ("Typeflag", box 0); ("Uname", box "guest"); ("Gname", box "guest")]
        tarWriteHeader w hdr
        tarWrite w data
        __ret
    with
        | Return -> __ret
main()
