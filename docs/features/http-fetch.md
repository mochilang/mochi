## HTTP Fetch

Retrieve JSON over HTTP using `fetch`. The expression returns a value of the requested type and automatically decodes the response body.

```mochi
type Todo {
  userId: int
  id: int
  title: string
  completed: bool
}

let todo = fetch "https://example.com/todos/1" as Todo
```

The `with` clause provides options such as the HTTP method, headers, and body. The body may be any serializable value and will be encoded as JSON by default.

```mochi
let created: Todo = fetch "https://example.com/todos" with {
  method: "POST",
  headers: {
    "Content-Type": "application/json"
  },
  body: todo
}
```

`fetch` raises an error if the request fails or the response cannot be decoded into the expected type. Handle errors using `try`/`catch` blocks.

```mochi
try {
  let result = fetch "https://example.com/404" as Todo
  print(result)
} catch err {
  print("request failed", err)
}
```
