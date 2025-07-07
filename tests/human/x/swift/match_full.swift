let x = 2
let label: String
switch x {
case 1: label = "one"
case 2: label = "two"
case 3: label = "three"
default: label = "unknown"
}
print(label)

let day = "sun"
let mood: String
switch day {
case "mon": mood = "tired"
case "fri": mood = "excited"
case "sun": mood = "relaxed"
default: mood = "normal"
}
print(mood)

let ok = true
let status = ok ? "confirmed" : "denied"
print(status)

func classify(_ n: Int) -> String {
    switch n {
    case 0: return "zero"
    case 1: return "one"
    default: return "many"
    }
}
print(classify(0))
