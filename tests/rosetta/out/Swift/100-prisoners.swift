// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:35:06Z
import Foundation

func shuffle(_ xs: [Int]) -> [Int] {
    var arr = xs
    var i = 99
    while i > 0 {
        let j = Int(Date().timeIntervalSince1970 * 1000000000) % (i + 1)
        let tmp = arr[i]
        arr[i] = arr[j]
        arr[j] = tmp
        i = i - 1
    }
    return arr
}
func doTrials(_ trials: Int, _ np: Int, _ strategy: String) {
    var pardoned = 0
    var t = 0
    while t < trials {
        var drawers: [Int] = []
        var i = 0
        while i < 100 {
            drawers = drawers + [i]
            i = i + 1
        }
        drawers = shuffle(drawers)
        var p = 0
        var success = true
        while p < np {
            var found = false
            if strategy == "optimal" {
                var prev = p
                var d = 0
                while d < 50 {
                    let this = drawers[prev]
                    if this == p {
                        found = true
                        break
                    }
                    prev = this
                    d = d + 1
                }
            }
            else {
                var opened: [Bool] = []
                var k = 0
                while k < 100 {
                    opened = opened + [false]
                    k = k + 1
                }
                var d = 0
                while d < 50 {
                    var n = Int(Date().timeIntervalSince1970 * 1000000000) % 100
                    while opened[n] {
                        n = Int(Date().timeIntervalSince1970 * 1000000000) % 100
                    }
                    opened[n] = true
                    if drawers[n] == p {
                        found = true
                        break
                    }
                    d = d + 1
                }
            }
            if !found {
                success = false
                break
            }
            p = p + 1
        }
        if success {
            pardoned = pardoned + 1
        }
        t = t + 1
    }
    let rf = Double((Double(pardoned)) / (Double(trials))) * 100.0
    print("  strategy = " + strategy + "  pardoned = " + String(pardoned) + " relative frequency = " + String(rf) + "%")
}
func main() {
    let trials = 1000
    for np in [10, 100] {
        print("Results from " + String(trials) + " trials with " + String(np) + " prisoners:\n")
        for strat in ["random", "optimal"] {
            doTrials(trials, np, strat)
        }
    }
}
main()
