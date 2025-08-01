// Generated 2025-08-01 21:11 +0700

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
type FileInfo = {
    name: string
    size: int
    kind: string
}
type FTPConn = {
    dir: string
}
let serverData: Map<string, Map<string, string>> = Map.ofList [("pub", Map.ofList [("somefile.bin", "This is a file from the FTP server."); ("readme.txt", "Hello from ftp.")])]
let serverNames: Map<string, string array> = Map.ofList [("pub", [|"somefile.bin"; "readme.txt"|])]
let rec connect (hostport: string) =
    let mutable __ret : FTPConn = Unchecked.defaultof<FTPConn>
    let mutable hostport = hostport
    try
        printfn "%s" ("Connected to " + hostport)
        __ret <- { dir = "/" }
        raise Return
        __ret
    with
        | Return -> __ret
and login (conn: FTPConn) (user: string) (pass: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable conn = conn
    let mutable user = user
    let mutable pass = pass
    try
        printfn "%s" ("Logged in as " + user)
        __ret
    with
        | Return -> __ret
and changeDir (conn: FTPConn) (dir: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable conn = conn
    let mutable dir = dir
    try
        conn <- { conn with dir = dir }
        __ret
    with
        | Return -> __ret
and list (conn: FTPConn) =
    let mutable __ret : FileInfo array = Unchecked.defaultof<FileInfo array>
    let mutable conn = conn
    try
        let names: string array = serverNames.[(conn.dir)]
        let dataDir: string> = serverData.[(conn.dir)]
        let mutable out: FileInfo array = [||]
        for name in names do
            let content: string> = dataDir.[name]
            out <- Array.append out [|{ name = name; size = Seq.length content; kind = "file" }|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and retrieve (conn: FTPConn) (name: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable conn = conn
    let mutable name = name
    try
        __ret <- unbox<string> ((serverData.[(conn.dir)]).[name])
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let conn: FTPConn = connect "localhost:21"
        login conn "anonymous" "anonymous"
        changeDir conn "pub"
        printfn "%s" (conn.dir)
        let files: FileInfo array = list conn
        for f in files do
            printfn "%s" (((f.name) + " ") + (string (f.size)))
        let data: string = retrieve conn "somefile.bin"
        printfn "%s" (("Wrote " + (string (String.length data))) + " bytes to somefile.bin")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
