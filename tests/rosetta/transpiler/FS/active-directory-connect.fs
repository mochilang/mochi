// Generated 2025-07-24 20:52 +0700

exception Return

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
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let client: LDAPClient = { Base = "dc=example,dc=com"; Host = "ldap.example.com"; Port = 389; UseSSL = false; BindDN = "uid=readonlyuser,ou=People,dc=example,dc=com"; BindPassword = "readonlypassword"; UserFilter = "(uid=%s)"; GroupFilter = "(memberUid=%s)"; Attributes = [|"givenName"; "sn"; "mail"; "uid"|] }
        if connect client then
            printfn "%s" ("Connected to " + (client.Host))
        else
            printfn "%s" "Failed to connect"
        __ret
    with
        | Return -> __ret
main()
