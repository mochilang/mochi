// Generated 2025-07-25 01:11 +0700

let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let arr: string array = [|"apple"; "orange"; "pear"|]
        printfn "%s" (((("Length of " + (string arr)) + " is ") + (string (Array.length arr))) + ".")
        __ret
    with
        | Return -> __ret
main()
