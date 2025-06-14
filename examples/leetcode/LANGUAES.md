# Mochi Syntax Reference for LeetCode Examples

This short guide summarizes the language features used in the example solutions under this directory. Reading through it should help you understand the code in each problem folder.

## Basics

- **Comments** start with `//`.
- Use `fun` to declare a function. Parameters and return types are required.

```mochi
fun twoSum(nums: list<int>, target: int): list<int> {
  // ...
}
```
- `let` defines an immutable value, while `var` creates a mutable variable.

```mochi
let n = len(nums)
var i = 0
```

## Control Flow

- Conditional statements use `if`/`else`.
- Loops are written with `for` or `while`.
- Ranges `0..n` iterate from 0 up to but not including `n`.

```mochi
for i in 0..n {
  if nums[i] == target {
    break
  }
}
```

## Collections

- Lists, maps, and sets are written as `list<T>`, `map<K,V>`, and `set<T>`.
- Append to a list with `+ [value]`.
- Access elements with `arr[index]` and slices with `arr[start:end]`.
- Map elements are read or written using `m[key]`.

```mochi
var counts: map<string, int> = {}
counts["a"] = 1
var seen: set<int> = {}
seen.add(3)
```

## Dataset Queries

Many solutions use Mochi’s query syntax to filter or sort lists.

```mochi
let sorted = from x in nums
             sort by x
             select x
```

Grouping is also supported:

```mochi
let counts = from ch in s
             group by ch into g
             select { ch: g.key, freq: count(g) }
```

## Built‑in Helpers

- `len(x)` returns the length of a list or string.
- `print(...)` writes output.
- Type conversions like `str(n)` are available.

## Testing

Each file ends with `test` blocks. Assertions use `expect`.

```mochi
test "example" {
  expect twoSum([2,7,11,15], 9) == [0,1]
}
```

Running `mochi test` will execute these cases.

## Common Pitfalls

The examples include comments about frequent mistakes such as:

- Using `=` instead of `==` for comparison.
- Reassigning a `let` variable (use `var` when mutation is required).
- Off‑by‑one errors in ranges (`0..n` is n items).
- Attempting Python syntax like `for i in range(n)`.

Refer back to the example files whenever you encounter unfamiliar syntax.
