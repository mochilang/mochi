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

let r: float = 3.0
let area = (math.pi) * (math.pow(r, 2.0))
let root = math.sqrt(49.0)
let sin45 = math.sin((math.pi) / 4.0)
let log_e = math.log(math.e)
printfn "%s" (String.concat " " [string "Circle area with r ="; sprintf "%.1f" r; string "=>"; string area])
printfn "%s" (String.concat " " [string "Square root of 49:"; string root])
printfn "%s" (String.concat " " [string "sin(π/4):"; string sin45])
printfn "%s" (String.concat " " [string "log(e):"; string log_e])
