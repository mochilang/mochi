// Generated 2025-08-02 11:33 +0700

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
type Window = {
    x: int
    y: int
    w: int
    h: int
    maximized: bool
    iconified: bool
    visible: bool
    shifted: bool
}
let rec showState (w: Window) (label: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable w = w
    let mutable label = label
    try
        printfn "%s" ((((((((((((((label + ": pos=(") + (string (w.x))) + ",") + (string (w.y))) + ") size=(") + (string (w.w))) + "x") + (string (w.h))) + ") max=") + (string (w.maximized))) + " icon=") + (string (w.iconified))) + " visible=") + (string (w.visible)))
        __ret
    with
        | Return -> __ret
and maximize (w: Window) =
    let mutable __ret : Window = Unchecked.defaultof<Window>
    let mutable w = w
    try
        w <- { w with maximized = true }
        w <- { w with w = 800 }
        w <- { w with h = 600 }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and unmaximize (w: Window) =
    let mutable __ret : Window = Unchecked.defaultof<Window>
    let mutable w = w
    try
        w <- { w with maximized = false }
        w <- { w with w = 640 }
        w <- { w with h = 480 }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and iconify (w: Window) =
    let mutable __ret : Window = Unchecked.defaultof<Window>
    let mutable w = w
    try
        w <- { w with iconified = true }
        w <- { w with visible = false }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and deiconify (w: Window) =
    let mutable __ret : Window = Unchecked.defaultof<Window>
    let mutable w = w
    try
        w <- { w with iconified = false }
        w <- { w with visible = true }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and hide (w: Window) =
    let mutable __ret : Window = Unchecked.defaultof<Window>
    let mutable w = w
    try
        w <- { w with visible = false }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and showWindow (w: Window) =
    let mutable __ret : Window = Unchecked.defaultof<Window>
    let mutable w = w
    try
        w <- { w with visible = true }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and move (w: Window) =
    let mutable __ret : Window = Unchecked.defaultof<Window>
    let mutable w = w
    try
        if w.shifted then
            w <- { w with x = (w.x) - 10 }
            w <- { w with y = (w.y) - 10 }
        else
            w <- { w with x = (w.x) + 10 }
            w <- { w with y = (w.y) + 10 }
        w <- { w with shifted = not (w.shifted) }
        __ret <- w
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable win: Window = { x = 100; y = 100; w = 640; h = 480; maximized = false; iconified = false; visible = true; shifted = false }
        showState (win) ("Start")
        win <- maximize (win)
        showState (win) ("Maximize")
        win <- unmaximize (win)
        showState (win) ("Unmaximize")
        win <- iconify (win)
        showState (win) ("Iconify")
        win <- deiconify (win)
        showState (win) ("Deiconify")
        win <- hide (win)
        showState (win) ("Hide")
        win <- showWindow (win)
        showState (win) ("Show")
        win <- move (win)
        showState (win) ("Move")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
