// Generated 2025-07-26 04:38 +0700

exception Return

let rec parseBool (s: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable s = s
    try
        let l: string = s.ToLower()
        if ((((l = "1") || (l = "t")) || (l = true)) || (l = "yes")) || (l = "y") then
            __ret <- true
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable n: bool = true
        printfn "%d" (if n then 1 else 0)
        printfn "%s" "bool"
        n <- not n
        printfn "%d" (if n then 1 else 0)
        let x: int = 5
        let y: int = 8
        printfn "%s" (String.concat " " [|sprintf "%A" "x == y:"; sprintf "%b" (x = y)|])
        printfn "%s" (String.concat " " [|sprintf "%A" "x < y:"; sprintf "%b" (x < y)|])
        printfn "%s" "\nConvert String into Boolean Data type\n"
        let str1: string = "japan"
        printfn "%s" (String.concat " " [|sprintf "%A" "Before :"; sprintf "%A" "string"|])
        let bolStr: bool = parseBool str1
        printfn "%s" (String.concat " " [|sprintf "%A" "After :"; sprintf "%A" "bool"|])
        __ret
    with
        | Return -> __ret
main()
