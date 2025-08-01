// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:57:11Z
func printStat(_ fs: [String: Bool], _ path: String) {
    if fs.contains(path) {
        if fs[path] {
            print(path + " is a directory")
        }
        else {
            print(path + " is a file")
        }
    }
    else {
        print("stat " + path + ": no such file or directory")
    }
}
func main() {
    var fs: [String: Bool] = []
    fs["docs"] = true
    for p in ["input.txt", "/input.txt", "docs", "/docs"] {
        printStat(fs, p)
    }
}
main()
