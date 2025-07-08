func sum_rec(_ n: Int, _ acc: Int) -> Int {
    if n == 0 {
        return acc
    }
    return sum_rec(n - 1, acc + n)
}
print(sum_rec(10, 0))
