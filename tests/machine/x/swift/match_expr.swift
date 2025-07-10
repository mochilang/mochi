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
