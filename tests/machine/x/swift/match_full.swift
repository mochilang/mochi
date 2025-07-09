let x = 2
let label = { () in
    switch x {
    case 1: return "one"
    case 2: return "two"
    case 3: return "three"
    default: return "unknown"
    }
}()
print(label)
let day = "sun"
let mood = { () in
    switch day {
    case "mon": return "tired"
    case "fri": return "excited"
    case "sun": return "relaxed"
    default: return "normal"
    }
}()
print(mood)
let ok = true
let status = { () in
    switch ok {
    case true: return "confirmed"
    case false: return "denied"
    }
}()
print(status)
func classify(_ n: Int) -> String {
    return { () in
    switch n {
    case 0: return "zero"
    case 1: return "one"
    default: return "many"
    }
}()
}
print(classify(0))
print(classify(5))
