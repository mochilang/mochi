// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

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
open System

let w: int = 400
let h: int = 300
let n: int = 15000
let frost: int = 255
let mutable grid: int array array = [||]
let mutable y: int = 0
while y < h do
    let mutable row: int array = [||]
    let mutable x: int = 0
    while x < w do
        row <- Array.append row [|0|]
        x <- x + 1
    grid <- Array.append grid [|row|]
    y <- y + 1
(grid.[h / 3]).[w / 3] <- frost
let rec inBounds (x: int) (y: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable x = x
    let mutable y = y
    try
        __ret <- (((x >= 0) && (x < w)) && (y >= 0)) && (y < h)
        raise Return
        __ret
    with
        | Return -> __ret
and hasNeighbor (x: int) (y: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable x = x
    let mutable y = y
    try
        let mutable dy: int = -1
        while dy <= 1 do
            let mutable dx: int = -1
            while dx <= 1 do
                if not ((dx = 0) && (dy = 0)) then
                    let nx: int = x + dx
                    let ny: int = y + dy
                    if (unbox<bool> (inBounds nx ny)) && ((int ((grid.[ny]).[nx])) = frost) then
                        __ret <- true
                        raise Return
                dx <- dx + 1
            dy <- dy + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let mutable a: int = 0
try
    while a < n do
        let mutable px: int = (((_now()) % w + w) % w)
        let mutable py: int = (((_now()) % h + h) % h)
        if (int ((grid.[py]).[px])) = frost then
            let mutable lost: bool = false
            try
                while true do
                    px <- (int (px + (int ((((_now()) % 3 + 3) % 3))))) - 1
                    py <- (int (py + (int ((((_now()) % 3 + 3) % 3))))) - 1
                    if not (inBounds px py) then
                        lost <- true
                        raise Break
                    if (int ((grid.[py]).[px])) <> frost then
                        raise Break
            with
            | Break -> ()
            | Continue -> ()
            if lost then
                raise Continue
        else
            let mutable lost: bool = false
            try
                while not (hasNeighbor px py) do
                    px <- (int (px + (int ((((_now()) % 3 + 3) % 3))))) - 1
                    py <- (int (py + (int ((((_now()) % 3 + 3) % 3))))) - 1
                    if not (inBounds px py) then
                        lost <- true
                        raise Break
            with
            | Break -> ()
            | Continue -> ()
            if lost then
                raise Continue
        (grid.[py]).[px] <- frost
        a <- a + 1
with
| Break -> ()
| Continue -> ()
