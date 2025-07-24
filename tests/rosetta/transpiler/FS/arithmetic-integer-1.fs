// Generated 2025-07-25 01:11 +0700

let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let a: int = 12
        let b: int = 8
        printfn "%s" (((((string a) + " + ") + (string b)) + " = ") + (string (a + b)))
        printfn "%s" (((((string a) + " - ") + (string b)) + " = ") + (string (a - b)))
        printfn "%s" (((((string a) + " * ") + (string b)) + " = ") + (string (a * b)))
        printfn "%s" (((((string a) + " / ") + (string b)) + " = ") + (string (int (a / b))))
        printfn "%s" (((((string a) + " % ") + (string b)) + " = ") + (string (a % b)))
        __ret
    with
        | Return -> __ret
main()
