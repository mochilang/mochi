// Generated 2025-07-21 22:29 +0700

open System

module math
let pi: float = System.Math.PI
let e: float = System.Math.E
let rec sqrt x =
    System.Math.Sqrt x
let rec pow x y =
    System.Math.Pow x y
let rec sin x =
    System.Math.Sin x
let rec log x =
    System.Math.Log x

printfn "%s" (string (math.sqrt(16.0)))
printfn "%s" (string (math.pi))
