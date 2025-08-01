// Generated 2025-08-01 18:27 +0700

exception Return

let mutable nextPID: int = 1
let rec run (hasArg: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable hasArg = hasArg
    try
        let pid: int = nextPID
        nextPID <- nextPID + 1
        printfn "%s" ("PID: " + (string pid))
        if not hasArg then
            printfn "%s" "Done."
            __ret <- ()
            raise Return
        let childPID: int = nextPID
        printfn "%s" ("Child's PID: " + (string childPID))
        run false
        __ret
    with
        | Return -> __ret
run true
