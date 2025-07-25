// Generated by Mochi compiler v0.10.26 on 2025-07-16T13:16:20Z
let PI = 3.141592653589793
func sinApprox(_ x: Double) -> Double {
    var term = x
    var sum = x
    var n = 1
    while n <= 10 {
        let denom = Double(((2 * n) * (2 * n + 1)))
        term = -term * x * x / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}
func cosApprox(_ x: Double) -> Double {
    var term = 1.0
    var sum = 1.0
    var n = 1
    while n <= 10 {
        let denom = Double(((2 * n - 1) * (2 * n)))
        term = -term * x * x / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}
func sqrtApprox(_ x: Double) -> Double {
    var guess = x
    var i = 0
    while i < 10 {
        guess = (guess + x / guess) / 2.0
        i = i + 1
    }
    return guess
}
let L = 10.0
let G = 9.81
let dt = 0.2
let phi0 = PI / 4.0
let omega = sqrtApprox(G / L)
var t = 0.0
for step in 0..<10 {
    let phi = phi0 * cosApprox(omega * t)
    let pos = Int((10.0 * sinApprox(phi) + 0.5))
    print(String(pos))
    t = t + dt
}
