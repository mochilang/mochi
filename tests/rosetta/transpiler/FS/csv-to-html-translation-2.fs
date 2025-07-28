// Generated 2025-07-28 11:14 +0700

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
