import Foundation

func _indexString(_ s: String, _ i: Int) -> String {
    var idx = i
    let chars = Array(s)
    if idx < 0 { idx += chars.count }
    if idx < 0 || idx >= chars.count { fatalError("index out of range") }
    return String(chars[idx])
}

func _sliceString(_ s: String, _ i: Int, _ j: Int) -> String {
    var start = i
    var end = j
    let chars = Array(s)
    let n = chars.count
    if start < 0 { start += n }
    if end < 0 { end += n }
    if start < 0 { start = 0 }
    if end > n { end = n }
    if end < start { end = start }
    return String(chars[start..<end])
}

func expand(_ s: String, _ left: Int, _ right: Int) -> Int {
    var l = left
    var r = right
    let n = s.count
    while l >= 0 && r < n {
        if _indexString(s, l) != _indexString(s, r) {
            break
        }
        l -= 1
        r += 1
    }
    return r - l - 1
}

func longestPalindrome(_ s: String) -> String {
    if s.count <= 1 {
        return s
    }
    var start = 0
    var end = 0
    let n = s.count
    for i in 0..<n {
        let len1 = expand(s, i, i)
        let len2 = expand(s, i, i + 1)
        var l = len1
        if len2 > len1 { l = len2 }
        if l > end - start {
            start = i - (l - 1) / 2
            end = i + l / 2
        }
    }
    return _sliceString(s, start, end + 1)
}

func example_1() {
    let ans = longestPalindrome("babad")
    assert(ans == "bab" || ans == "aba")
}
func example_2() { assert(longestPalindrome("cbbd") == "bb") }
func single_char() { assert(longestPalindrome("a") == "a") }
func two_chars() {
    let ans = longestPalindrome("ac")
    assert(ans == "a" || ans == "c")
}

func main() {
    example_1()
    example_2()
    single_char()
    two_chars()
}
main()
