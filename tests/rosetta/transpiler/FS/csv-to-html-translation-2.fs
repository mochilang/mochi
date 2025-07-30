// Generated 2025-07-30 21:41 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let c: string = (((("Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n") + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") + "The multitude,Who are you?\n") + "Brians mother,I'm his mother; that's who!\n") + "The multitude,Behold his mother! Behold his mother!"
let mutable rows: string array array = [||]
for line in c.Split([|"\n"|], System.StringSplitOptions.None) do
    rows <- Array.append rows [|unbox<string array> (line.Split([|","|], System.StringSplitOptions.None))|]
let headings: bool = true
printfn "%s" "<table>"
if headings then
    if (Seq.length rows) > 0 then
        let mutable th: string = ""
        for h in rows.[0] do
            th <- ((th + "<th>") + h) + "</th>"
        printfn "%s" "   <thead>"
        printfn "%s" (("      <tr>" + th) + "</tr>")
        printfn "%s" "   </thead>"
        printfn "%s" "   <tbody>"
        let mutable i: int = 1
        while i < (Seq.length rows) do
            let mutable cells: string = ""
            for cell in rows.[i] do
                cells <- ((cells + "<td>") + cell) + "</td>"
            printfn "%s" (("      <tr>" + cells) + "</tr>")
            i <- i + 1
        printfn "%s" "   </tbody>"
else
    for row in rows do
        let mutable cells: string = ""
        for cell in row do
            cells <- ((cells + "<td>") + cell) + "</td>"
        printfn "%s" (("    <tr>" + cells) + "</tr>")
printfn "%s" "</table>"
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
