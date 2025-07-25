// Generated 2025-07-26 04:38 +0700

exception Return

type Foobar = {
    Exported: int
    unexported: int
}
let rec examineAndModify (f: Foobar) =
    let mutable __ret : Foobar = Unchecked.defaultof<Foobar>
    let mutable f = f
    try
        printfn "%s" ((((((((" v: {" + (string (f.Exported))) + " ") + (string (f.unexported))) + "} = {") + (string (f.Exported))) + " ") + (string (f.unexported))) + "}")
        printfn "%s" "    Idx Name       Type CanSet"
        printfn "%s" "     0: Exported   int  true"
        printfn "%s" "     1: unexported int  false"
        f <- { f with Exported = 16 }
        f <- { f with unexported = 44 }
        printfn "%s" "  modified unexported field via unsafe"
        __ret <- f
        raise Return
        __ret
    with
        | Return -> __ret
and anotherExample () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" "bufio.ReadByte returned error: unsafely injected error value into bufio inner workings"
        __ret
    with
        | Return -> __ret
let mutable obj: Foobar = { Exported = 12; unexported = 42 }
printfn "%s" (((("obj: {" + (string (obj.Exported))) + " ") + (string (obj.unexported))) + "}")
obj <- examineAndModify obj
printfn "%s" (((("obj: {" + (string (obj.Exported))) + " ") + (string (obj.unexported))) + "}")
anotherExample()
