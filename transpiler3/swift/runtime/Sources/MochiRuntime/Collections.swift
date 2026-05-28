// MochiRuntime collection helpers matching Mochi semantics.
import Foundation

// mochiAppend returns a new array with elem appended (functional semantics).
public func mochiAppend<T>(_ xs: [T], _ elem: T) -> [T] {
    var tmp = xs
    tmp.append(elem)
    return tmp
}

// mochiSetAdd returns a new set with elem inserted (functional semantics).
public func mochiSetAdd<T: Hashable>(_ s: Set<T>, _ elem: T) -> Set<T> {
    var tmp = s
    tmp.insert(elem)
    return tmp
}

// mochiMapKeys returns a sorted array of keys (matches Mochi vm3 sort-on-iteration).
public func mochiMapKeys<K: Comparable & Hashable, V>(_ m: [K: V]) -> [K] {
    return m.keys.sorted()
}

// mochiMapValues returns values sorted by key (matches Mochi vm3 sort-on-iteration).
public func mochiMapValues<K: Comparable & Hashable, V>(_ m: [K: V]) -> [V] {
    return m.keys.sorted().map { m[$0]! }
}

// mochiAbsInt returns the absolute value of an Int64.
public func mochiAbsInt(_ n: Int64) -> Int64 {
    return n < 0 ? -n : n
}

// mochiAbsFloat returns the absolute value of a Double.
public func mochiAbsFloat(_ f: Double) -> Double {
    return f < 0 ? -f : f
}

// mochiFloor returns the floor of a Double as Int64.
public func mochiFloor(_ f: Double) -> Int64 {
    return Int64(f.rounded(.down))
}

// mochiCeil returns the ceiling of a Double as Int64.
public func mochiCeil(_ f: Double) -> Int64 {
    return Int64(f.rounded(.up))
}

// mochiStr converts a scalar value to its string representation matching Mochi vm3.
public func mochiStr(_ n: Int64) -> String {
    return "\(n)"
}

public func mochiStr(_ f: Double) -> String {
    if f.isNaN { return "NaN" }
    if f.isInfinite { return f > 0 ? "+Inf" : "-Inf" }
    if f.truncatingRemainder(dividingBy: 1) == 0 && f >= -9007199254740992 && f <= 9007199254740992 {
        return "\(Int64(f))"
    }
    return "\(f)"
}

public func mochiStr(_ b: Bool) -> String {
    return b ? "true" : "false"
}

public func mochiStr(_ s: String) -> String {
    return s
}

// mochiStrSubstring returns the substring of s from start to end (rune indices).
public func mochiStrSubstring(_ s: String, _ start: Int64, _ end: Int64) -> String {
    let scalars = s.unicodeScalars
    let count = scalars.count
    let si = max(0, min(Int(start), count))
    let ei = max(si, min(Int(end), count))
    let startIdx = scalars.index(scalars.startIndex, offsetBy: si)
    let endIdx = scalars.index(scalars.startIndex, offsetBy: ei)
    return String(scalars[startIdx..<endIdx])
}

// mochiStrReverse returns the string with codepoints reversed.
public func mochiStrReverse(_ s: String) -> String {
    return String(s.unicodeScalars.reversed().map { Character($0) })
}

// mochiStrSplit splits s by sep, returning [String].
public func mochiStrSplit(_ s: String, _ sep: String) -> [String] {
    if sep.isEmpty {
        return s.unicodeScalars.map { String($0) }
    }
    return s.components(separatedBy: sep)
}

// mochiStrJoin joins xs with sep.
public func mochiStrJoin(_ xs: [String], _ sep: String) -> String {
    return xs.joined(separator: sep)
}
