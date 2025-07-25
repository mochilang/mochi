// Generated 2025-07-25 09:57 +0000

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
type SomeStruct = {
    runtimeFields: Map<string, string>
}
open System

let rec main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
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
            ss <- { ss with runtimeFields = fields }
            printfn "%s" "\n"
            i <- i + 1
        while true do
            printfn "%s" "Which field do you want to inspect ? "
            let name: string = System.Console.ReadLine()
            if Map.containsKey name (ss.runtimeFields) then
                let value: string = (defaultArg (Map.tryFind name ss.runtimeFields) Unchecked.defaultof<string>)
                printfn "%s" (("Its value is '" + value) + "'")
                __ret <- ()
                raise Return
            else
                printfn "%s" "There is no field of that name, try again\n"
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
