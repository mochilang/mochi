fun main() {
    val nums = mutableListOf(1, 2, 3)
    val result = run {
        val _res = mutableListOftype_arguments
        for (n in nums) {
            if (n > 1) {
                _res.add(n.sum())
            }
        }
        _res
    }
    println(result)
}
