// MochiRuntime print functions matching vm3 (Go) output format exactly.

public func mochiPrint(_ value: String) {
    print(value)
}

public func mochiPrint(_ value: Int64) {
    print(value)
}

public func mochiPrint(_ value: Double) {
    if value.isNaN { print("NaN"); return }
    if value.isInfinite { print(value > 0 ? "+Inf" : "-Inf"); return }
    // Match Go strconv.FormatFloat(v, 'g', -1, 64):
    // whole numbers print without decimal point (4.0 → "4")
    if value.truncatingRemainder(dividingBy: 1) == 0 && value >= -9007199254740992 && value <= 9007199254740992 {
        print(Int64(value))
        return
    }
    // For non-whole-number values, Swift's String(Double) matches Go's %g
    // for the common range (no trailing zeros, shortest representation).
    print(value)
}

public func mochiPrint(_ value: Bool) {
    print(value ? "true" : "false")
}

// String helpers matching Mochi builtins.

public func mochiStrLen(_ s: String) -> Int64 {
    return Int64(s.unicodeScalars.count)
}

public func mochiStrIndex(_ s: String, _ i: Int64) -> String {
    let scalars = s.unicodeScalars
    let idx = scalars.index(scalars.startIndex, offsetBy: Int(i))
    return String(scalars[idx])
}

public func mochiStrContains(_ s: String, _ sub: String) -> Bool {
    return s.contains(sub)
}

public func mochiIntToFloat(_ n: Int64) -> Double {
    return Double(n)
}

public func mochiFloatToInt(_ f: Double) -> Int64 {
    return Int64(f)
}
