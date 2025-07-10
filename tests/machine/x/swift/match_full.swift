let x = 2
let label = { () in
    let __t = x
    switch __t {
    case 1: return "one"
    case 2: return "two"
    case 3: return "three"
    default: return "unknown"
    }
}()
print(label)
let day = "sun"
let mood = { () in
    let __t = day
    switch __t {
    case "mon": return "tired"
    case "fri": return "excited"
    case "sun": return "relaxed"
    default: return "normal"
    }
}()
print(mood)
let ok = true
let status = { () in
    let __t = ok
    switch __t {
    case true: return "confirmed"
    case false: return "denied"
    }
}()
print(status)
func classify(_ n: Int) -> String {
    return { () in
    let __t = n
    switch __t {
    case 0: return "zero"
    case 1: return "one"
    default: return "many"
    }
}()
}
print(classify(0))
print(classify(5))
