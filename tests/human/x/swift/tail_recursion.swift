func sumRec(_ n: Int, _ acc: Int) -> Int {
    if n == 0 { return acc }
    return sumRec(n - 1, acc + n)
}
print(sumRec(10, 0))
