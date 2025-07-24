// Generated 2025-07-25 01:11 +0700

let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let x: int = 43
        if x <> 42 then
            printfn "%s" "Assertion failed"
        else
            printfn "%s" "Assertion passed"
        __ret
    with
        | Return -> __ret
main()
