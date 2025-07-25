// Generated 2025-07-25 09:57 +0000

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
type LDAPClient = {
    Base: string
    Host: string
    Port: int
    UseSSL: bool
    BindDN: string
    BindPassword: string
    UserFilter: string
    GroupFilter: string
    Attributes: string array
}
let rec connect (client: LDAPClient) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable client = client
    try
        __ret <- ((client.Host) <> "") && ((client.Port) > 0)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let client: LDAPClient = { Base = "dc=example,dc=com"; Host = "ldap.example.com"; Port = 389; UseSSL = false; BindDN = "uid=readonlyuser,ou=People,dc=example,dc=com"; BindPassword = "readonlypassword"; UserFilter = "(uid=%s)"; GroupFilter = "(memberUid=%s)"; Attributes = [|"givenName"; "sn"; "mail"; "uid"|] }
        if connect client then
            printfn "%s" ("Connected to " + (client.Host))
        else
            printfn "%s" "Failed to connect"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
