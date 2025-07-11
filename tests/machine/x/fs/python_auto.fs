open System

module math
let pi : float = System.Math.PI
let e : float = System.Math.E
let sqrt (x: float) : float = System.Math.Sqrt x
let pow (x: float) (y: float) : float = System.Math.Pow(x, y)
let sin (x: float) : float = System.Math.Sin x
let log (x: float) : float = System.Math.Log x

printfn "%A" (math.sqrt(16))
printfn "%A" (math.pi)
