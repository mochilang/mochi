// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:55:46Z
func mkAdd(_ a: Int) -> (Int) -> Int {
    return { (b: Int) -> Int in a + b }
}
func mysum(_ x: Int, _ y: Int) -> Int {
    return x + y
}
func partialSum(_ x: Int) -> (Int) -> Int {
    return { (y: Int) -> Int in mysum(x, y) }
}
func main() {
    let add2 = mkAdd(2)
    let add3 = mkAdd(3)
    print(String(add2(5)) + " " + String(add3(6)))
    let partial = partialSum(13)
    print(String(partial(5)))
}
main()
