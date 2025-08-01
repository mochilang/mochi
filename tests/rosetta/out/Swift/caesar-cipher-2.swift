// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:55:44Z
func indexOf(_ s: String, _ ch: String) -> Int {
    var i = 0
    while i < s.count {
        if String(s[s.index(s.startIndex, offsetBy: i)..<s.index(s.startIndex, offsetBy: i + 1)]) == ch {
            return i
        }
        i = i + 1
    }
    return -1
}
func ord(_ ch: String) -> Int {
    let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let lower = "abcdefghijklmnopqrstuvwxyz"
    var idx = indexOf(upper, ch)
    if idx >= 0 {
        return 65 + idx
    }
    idx = indexOf(lower, ch)
    if idx >= 0 {
        return 97 + idx
    }
    return 0
}
func chr(_ n: Int) -> String {
    let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let lower = "abcdefghijklmnopqrstuvwxyz"
    if n >= 65 && n < 91 {
        return String(upper[upper.index(upper.startIndex, offsetBy: n - 65)..<upper.index(upper.startIndex, offsetBy: n - 64)])
    }
    if n >= 97 && n < 123 {
        return String(lower[lower.index(lower.startIndex, offsetBy: n - 97)..<lower.index(lower.startIndex, offsetBy: n - 96)])
    }
    return "?"
}
func shiftRune(_ r: String, _ k: Int) -> String {
    if r >= "a" && r <= "z" {
        return chr(((ord(r) - 97 + k) % 26) + 97)
    }
    if r >= "A" && r <= "Z" {
        return chr(((ord(r) - 65 + k) % 26) + 65)
    }
    return r
}
func encipher(_ s: String, _ k: Int) -> String {
    var out = ""
    var i = 0
    while i < s.count {
        out = out + shiftRune(Array(s[i..<i + 1]), k)
        i = i + 1
    }
    return out
}
func decipher(_ s: String, _ k: Int) -> String {
    return encipher(s, (26 - k % 26) % 26)
}
func main() {
    let pt = "The five boxing wizards jump quickly"
    print("Plaintext: " + pt)
    for key in [0, 1, 7, 25, 26] {
        if key < 1 || key > 25 {
            print("Key " + String(key) + " invalid")
            continue
        }
        let ct = encipher(pt, key)
        print("Key " + String(key))
        print("  Enciphered: " + ct)
        print("  Deciphered: " + decipher(ct, key))
    }
}
main()
