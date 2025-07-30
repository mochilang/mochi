// Generated 2025-07-31 00:10 +0700

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
type TinyInt = {
    value: int
}
let rec TinyInt_Add (self: TinyInt) (t2: TinyInt) =
    let mutable __ret : TinyInt = Unchecked.defaultof<TinyInt>
    let mutable self = self
    let mutable t2 = t2
    try
        let mutable value: int = self.value
        __ret <- NewTinyInt (value + (t2.value))
        raise Return
        __ret
    with
        | Return -> __ret
and TinyInt_Sub (self: TinyInt) (t2: TinyInt) =
    let mutable __ret : TinyInt = Unchecked.defaultof<TinyInt>
    let mutable self = self
    let mutable t2 = t2
    try
        let mutable value: int = self.value
        __ret <- NewTinyInt (value - (t2.value))
        raise Return
        __ret
    with
        | Return -> __ret
and TinyInt_Mul (self: TinyInt) (t2: TinyInt) =
    let mutable __ret : TinyInt = Unchecked.defaultof<TinyInt>
    let mutable self = self
    let mutable t2 = t2
    try
        let mutable value: int = self.value
        __ret <- NewTinyInt (value * (t2.value))
        raise Return
        __ret
    with
        | Return -> __ret
and TinyInt_Div (self: TinyInt) (t2: TinyInt) =
    let mutable __ret : TinyInt = Unchecked.defaultof<TinyInt>
    let mutable self = self
    let mutable t2 = t2
    try
        let mutable value: int = self.value
        __ret <- NewTinyInt (value / (t2.value))
        raise Return
        __ret
    with
        | Return -> __ret
and TinyInt_Rem (self: TinyInt) (t2: TinyInt) =
    let mutable __ret : TinyInt = Unchecked.defaultof<TinyInt>
    let mutable self = self
    let mutable t2 = t2
    try
        let mutable value: int = self.value
        __ret <- NewTinyInt (((value % (t2.value) + (t2.value)) % (t2.value)))
        raise Return
        __ret
    with
        | Return -> __ret
and TinyInt_Inc (self: TinyInt) =
    let mutable __ret : TinyInt = Unchecked.defaultof<TinyInt>
    let mutable self = self
    try
        let mutable value: int = self.value
        __ret <- unbox<TinyInt> (Add (NewTinyInt 1))
        raise Return
        __ret
    with
        | Return -> __ret
and TinyInt_Dec (self: TinyInt) =
    let mutable __ret : TinyInt = Unchecked.defaultof<TinyInt>
    let mutable self = self
    try
        let mutable value: int = self.value
        __ret <- unbox<TinyInt> (Sub (NewTinyInt 1))
        raise Return
        __ret
    with
        | Return -> __ret
and NewTinyInt (i: int) =
    let mutable __ret : TinyInt = Unchecked.defaultof<TinyInt>
    let mutable i = i
    try
        if i < 1 then
            i <- 1
        else
            if i > 10 then
                i <- 10
        __ret <- { value = i }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let t1: TinyInt = NewTinyInt 6
        let t2: TinyInt = NewTinyInt 3
        printfn "%s" ("t1      = " + (string (t1.value)))
        printfn "%s" ("t2      = " + (string (t2.value)))
        printfn "%s" ("t1 + t2 = " + (string (((TinyInt_Add t1 t2 :?> TinyInt).value))))
        printfn "%s" ("t1 - t2 = " + (string (((TinyInt_Sub t1 t2 :?> TinyInt).value))))
        printfn "%s" ("t1 * t2 = " + (string (((TinyInt_Mul t1 t2 :?> TinyInt).value))))
        printfn "%s" ("t1 / t2 = " + (string (((TinyInt_Div t1 t2 :?> TinyInt).value))))
        printfn "%s" ("t1 % t2 = " + (string (((TinyInt_Rem t1 t2 :?> TinyInt).value))))
        printfn "%s" ("t1 + 1  = " + (string (((TinyInt_Inc t1 :?> TinyInt).value))))
        printfn "%s" ("t1 - 1  = " + (string (((TinyInt_Dec t1 :?> TinyInt).value))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
