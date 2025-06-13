# Common Language Errors

Mochi reports helpful diagnostics when code is invalid. Here are a few issues beginners often encounter.

## Reassigning Immutable Bindings

Variables declared with `let` cannot be reassigned. Using `var` allows mutation.

```mochi
let count = 1
count = count + 1  // compile-time error
```

As documented in the [variables guide](features/variables.md), attempting to reassign a `let` binding produces a compile-time error.

## Unhandled Fetch Failures

`fetch` requests raise an error if the request fails or the response cannot be decoded. Wrap calls in `try`/`catch` to handle failures.

```mochi
try {
  let data = fetch "https://example.com/api" as map<string, string>
  print(data)
} catch err {
  print("request failed", err)
}
```

See the [HTTP fetch docs](features/http-fetch.md) for more details.
