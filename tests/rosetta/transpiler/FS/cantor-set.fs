// Generated 2025-07-27 22:00 +0700

exception Break
exception Continue

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

type Anon1 = {
    start: int
    len: int
    index: int
}
let width: int = 81
let height: int = 5
let mutable lines: string array = [||]
for i in 0 .. (height - 1) do
    let mutable row: string = ""
    let mutable j: int = 0
    while j < width do
        row <- row + "*"
        j <- j + 1
    lines <- unbox<string array> (Array.append lines [|row|])
let rec setChar (s: string) (idx: int) (ch: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable idx = idx
    let mutable ch = ch
    try
        __ret <- ((_substring s 0 idx) + ch) + (_substring s (idx + 1) (String.length s))
        raise Return
        __ret
    with
        | Return -> __ret
let mutable stack: Map<string, int> array = [|Map.ofList [("start", 0); ("len", width); ("index", 1)]|]
try
    while (unbox<int> (Array.length stack)) > 0 do
        let mutable frame: Map<string, int> = stack.[(unbox<int> (Array.length stack)) - 1]
        stack <- unbox<Map<string, int> array> (Array.sub stack 0 ((unbox<int> ((unbox<int> (Array.length stack)) - 1)) - 0))
        let start: int = frame.["start"] |> unbox<int>
        let lenSeg: int = frame.["len"] |> unbox<int>
        let index: int = frame.["index"] |> unbox<int>
        let seg: int = unbox<int> (lenSeg / 3)
        if seg = 0 then
            raise Continue
        let mutable i: int = index
        while i < height do
            let mutable j: int = start + seg
            while j < (start + (2 * seg)) do
                lines.[i] <- setChar (unbox<string> (lines.[i])) j " "
                j <- j + 1
            i <- i + 1
        stack <- unbox<Map<string, int> array> (Array.append stack [|Map.ofList [("start", start); ("len", seg); ("index", index + 1)]|])
        stack <- unbox<Map<string, int> array> (Array.append stack [|Map.ofList [("start", start + (seg * 2)); ("len", seg); ("index", index + 1)]|])
with
| Break -> ()
| Continue -> ()
for line in lines do
    printfn "%A" line
