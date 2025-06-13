## Common Language Errors

The Mochi compiler reports clear diagnostics, but some mistakes come up often when writing code.

- Using `=` instead of `==` inside conditions. `=` performs assignment while `==` checks equality.
- Forgetting to update loop counters which can lead to infinite loops.
- Returning the wrong type from a function. Ensure the expression after `return` matches the declared return type.
- Accessing list elements out of bounds. Check `len(list)` when iterating with numeric indices.
- Reassigning a `let` binding. Use `var` for mutable values.

Carefully reading error messages usually points to the exact line and expression that needs fixing.
