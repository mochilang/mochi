// Generated 2025-07-28 11:14 +0700

let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable rows: int array array = [||]
        for i in 0 .. (4 - 1) do
            rows <- Array.append rows [|[|i * 3; (i * 3) + 1; (i * 3) + 2|]|]
        printfn "%s" "<table>"
        printfn "%s" "    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>"
        let mutable idx: int = 0
        for row in rows do
            printfn "%s" (((((((("    <tr><td>" + (string idx)) + "</td><td>") + (string (row.[0]))) + "</td><td>") + (string (row.[1]))) + "</td><td>") + (string (row.[2]))) + "</td></tr>")
            idx <- idx + 1
        printfn "%s" "</table>"
        __ret
    with
        | Return -> __ret
main()
