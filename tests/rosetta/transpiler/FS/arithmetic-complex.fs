// Generated 2025-07-25 00:53 +0700

exception Return

type Complex = {
    re: float
    im: float
}
let rec add (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { re = (a.re) + (b.re); im = (a.im) + (b.im) }
        raise Return
        __ret
    with
        | Return -> __ret
and mul (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { re = ((a.re) * (b.re)) - ((a.im) * (b.im)); im = ((a.re) * (b.im)) + ((a.im) * (b.re)) }
        raise Return
        __ret
    with
        | Return -> __ret
and neg (a: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    try
        __ret <- { re = -(a.re); im = -(a.im) }
        raise Return
        __ret
    with
        | Return -> __ret
and inv (a: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    try
        let denom: float = ((a.re) * (a.re)) + ((a.im) * (a.im))
        __ret <- { re = (a.re) / denom; im = (-(a.im)) / denom }
        raise Return
        __ret
    with
        | Return -> __ret
and conj (a: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    try
        __ret <- { re = a.re; im = -(a.im) }
        raise Return
        __ret
    with
        | Return -> __ret
and cstr (a: Complex) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    try
        let mutable s: string = "(" + (string (a.re))
        if (a.im) >= (float 0) then
            s <- ((s + "+") + (string (a.im))) + "i)"
        else
            s <- (s + (string (a.im))) + "i)"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let a: Complex = { re = 1.0; im = 1.0 }
let b: Complex = { re = 3.14159; im = 1.25 }
printfn "%s" ("a:       " + (cstr a))
printfn "%s" ("b:       " + (cstr b))
printfn "%s" ("a + b:   " + (cstr (add a b)))
printfn "%s" ("a * b:   " + (cstr (mul a b)))
printfn "%s" ("-a:      " + (cstr (neg a)))
printfn "%s" ("1 / a:   " + (cstr (inv a)))
printfn "%s" ("a̅:       " + (cstr (conj a)))
