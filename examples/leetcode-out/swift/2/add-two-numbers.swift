import Foundation

func addTwoNumbers(_ l1: [Int], _ l2: [Int]) -> [Int] {
    var i = 0
    var j = 0
    var carry = 0
    var result: [Int] = []
    while i < l1.count || j < l2.count || carry > 0 {
        var x = 0
        if i < l1.count {
            x = l1[i]
            i += 1
        }
        var y = 0
        if j < l2.count {
            y = l2[j]
            j += 1
        }
        let sum = x + y + carry
        let digit = sum % 10
        carry = sum / 10
        result.append(digit)
    }
    return result
}

func example_1() { assert(addTwoNumbers([2,4,3], [5,6,4]) == [7,0,8]) }
func example_2() { assert(addTwoNumbers([0], [0]) == [0]) }
func example_3() { assert(addTwoNumbers([9,9,9,9,9,9,9], [9,9,9,9]) == [8,9,9,9,0,0,0,1]) }

func main() {
    example_1()
    example_2()
    example_3()
}
main()
