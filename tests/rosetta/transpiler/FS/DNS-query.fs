// Generated 2025-07-24 08:25 +0000

exception Return

open System

module net =
    open System.Net

    let rec LookupHost host =
        let mutable __ret : obj array = Unchecked.defaultof<obj array>
        let mutable host = host
        try
            let addrs = Dns.GetHostAddresses host
            let mapped = Array.map (            fun ip -> (ip.ToString())) addrs
            let lst = Array.toList mapped
            __ret <- [|box lst; null|]
            raise Return
            __ret
        with
            | Return -> __ret

let res: obj array = net.LookupHost("www.kame.net")
let addrs = res.[0]
let err = res.[1]
if err = null then
    printfn "%s" (string addrs)
else
    printfn "%A" err
