// Generated 2025-07-24 20:52 +0700

exception Return

type SomeStruct = {
    runtimeFields: Map<string, string>
}
open System

let rec main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let mutable ss: SomeStruct = { runtimeFields = Map.ofList [] }
        printfn "%s" "Create two fields at runtime: \n"
        let mutable i: int = 1
        while i <= 2 do
            printfn "%s" (("  Field #" + (string i)) + ":\n")
            printfn "%s" "       Enter name  : "
            let name: string = System.Console.ReadLine()
            printfn "%s" "       Enter value : "
            let value: string = System.Console.ReadLine()
            let mutable fields: Map<string, string> = ss.runtimeFields
            fields <- Map.add name value fields
            { ss with runtimeFields = fields }
            printfn "%s" "\n"
            i <- i + 1
        while true do
            printfn "%s" "Which field do you want to inspect ? "
            let name: string = System.Console.ReadLine()
            if Seq.contains name (ss.runtimeFields) then
                let value = Map.find name ss.runtimeFields
                printfn "%s" (("Its value is '" + value) + "'")
                __ret <- ()
                raise Return
            else
                printfn "%s" "There is no field of that name, try again\n"
        __ret
    with
        | Return -> __ret
main()
