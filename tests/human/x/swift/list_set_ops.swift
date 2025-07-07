let union = Array(Set([1, 2]).union([2, 3])).sorted()
print(union)

let diff = Array(Set([1, 2, 3]).subtracting([2])).sorted()
print(diff)

let inter = Array(Set([1, 2, 3]).intersection([2, 4])).sorted()
print(inter)

let unionAll = [1, 2] + [2, 3]
print(unionAll.count)
