open System

module math
let pi : float = System.Math.PI
let e : float = System.Math.E
let sqrt (x: float) : float = System.Math.Sqrt x
let pow (x: float) (y: float) : float = System.Math.Pow(x, y)
let sin (x: float) : float = System.Math.Sin x
let log (x: float) : float = System.Math.Log x

let r: float = 3
let area: obj = math.pi * math.pow(r, 2)
let root: obj = math.sqrt(49)
let sin45: obj = math.sin(math.pi / 4)
let log_e: obj = math.log(math.e)
printfn "%s" (String.concat " " [string "Circle area with r ="; string r; string "=>"; string area])
printfn "%s" (String.concat " " [string "Square root of 49:"; string root])
printfn "%s" (String.concat " " [string "sin(π/4):"; string sin45])
printfn "%s" (String.concat " " [string "log(e):"; string log_e])
