# Common Mochi Language Errors

This short guide lists mistakes often made by new users of Mochi along with tips on how to fix them.

## Using `=` Instead of `==`

The equality operator in Mochi is `==`. Using a single `=` is an assignment and will cause a compile-time error.

```mochi
// Wrong
if x = 3 {
  // ...
}

// Correct
if x == 3 {
  // ...
}
```

## Mixing `let` and `var`

`let` bindings are immutable. Attempting to reassign them produces an error. Use `var` for mutable variables.

```mochi
let count = 1
// count = 2  # compile-time error

var total = 1
total = total + 1  // ok
```

## Off-by-One Loop Boundaries

Ranges like `0..n` iterate from `0` up to but not including `n`. Forgetting this can lead to missing the last element. When you need to include the last index, make sure to use `n` instead of `n-1` in your calculations.

```mochi
for i in 0..len(nums) {
  // accesses nums[i]
}
```

## Mutable List Updates

Appending to a list creates a new list. Remember to assign the result back if the list is stored in a `var`.

```mochi
var xs = []
xs = xs + [1]  // don't forget the assignment
```
