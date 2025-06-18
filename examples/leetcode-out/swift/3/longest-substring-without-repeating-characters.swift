import Foundation

func lengthOfLongestSubstring(_ s: String) -> Int {
    let n = s.count
    var start = 0
    var best = 0
    var i = 0
    let chars = Array(s)
    while i < n {
        var j = start
        while j < i {
            if chars[j] == chars[i] {
                start = j + 1
                break
            }
            j += 1
        }
        let length = i - start + 1
        if length > best {
            best = length
        }
        i += 1
    }
    return best
}

func example_1() { assert(lengthOfLongestSubstring("abcabcbb") == 3) }
func example_2() { assert(lengthOfLongestSubstring("bbbbb") == 1) }
func example_3() { assert(lengthOfLongestSubstring("pwwkew") == 3) }
func empty_string() { assert(lengthOfLongestSubstring("") == 0) }

func main() {
    example_1()
    example_2()
    example_3()
    empty_string()
}
main()
