// Generated 2025-07-24 20:52 +0700

exception Return

let rec search_user (directory: Map<string, string array>) (username: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable directory = directory
    let mutable username = username
    try
        __ret <- directory.[username]
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let client = Map.ofList [("Base", box "dc=example,dc=com"); ("Host", box "ldap.example.com"); ("Port", box 389); ("GroupFilter", box "(memberUid=%s)")]
        let directory = Map.ofList [("username", [|"admins"; "users"|]); ("john", [|"users"|])]
        let groups = search_user directory "username"
        if (Seq.length groups) > 0 then
            let mutable out: string = "Groups: ["
            let mutable i: int = 0
            while i < (Seq.length groups) do
                out <- ((out + "\"") + (groups.[i])) + "\""
                if i < ((Seq.length groups) - 1) then
                    out <- out + ", "
                i <- i + 1
            out <- out + "]"
            printfn "%s" out
        else
            printfn "%s" "User not found"
        __ret
    with
        | Return -> __ret
main()
