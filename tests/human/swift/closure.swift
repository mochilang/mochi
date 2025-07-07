func makeAdder(_ n: Int) -> (Int) -> Int {
    return { x in x + n }
}
let add10 = makeAdder(10)
print(add10(7))
