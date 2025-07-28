// Generated 2025-07-28 11:14 +0700

exception Return

let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec split (s: string) (sep: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    let mutable sep = sep
    try
        let mutable out: string array = [||]
        let mutable start: int = 0
        let mutable i: int = 0
        let n: int = String.length sep
        while i <= ((String.length s) - n) do
            if (_substring s i (i + n)) = sep then
                out <- Array.append out [|_substring s start i|]
                i <- i + n
                start <- i
            else
                i <- i + 1
        out <- Array.append out [|_substring s start (String.length s)|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and htmlEscape (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if ch = "&" then
                out <- out + "&amp;"
            else
                if ch = "<" then
                    out <- out + "&lt;"
                else
                    if ch = ">" then
                        out <- out + "&gt;"
                    else
                        out <- out + ch
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let c: string = (((("Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n") + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") + "The multitude,Who are you?\n") + "Brians mother,I'm his mother; that's who!\n") + "The multitude,Behold his mother! Behold his mother!"
let mutable rows: string array array = [||]
for line in c.Split([|"\n"|], System.StringSplitOptions.None) do
    rows <- Array.append rows [|unbox<string array> (line.Split([|","|], System.StringSplitOptions.None))|]
printfn "%s" "<table>"
for row in rows do
    let mutable cells: string = ""
    for cell in row do
        cells <- ((cells + "<td>") + (unbox<string> (htmlEscape cell))) + "</td>"
    printfn "%s" (("    <tr>" + cells) + "</tr>")
printfn "%s" "</table>"
