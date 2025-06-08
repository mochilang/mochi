## HTTP Fetch

Retrieve JSON over HTTP using `fetch`.

```mochi
type Todo {
  userId: int
  id: int
  title: string
  completed: bool
}

let todo = fetch "https://example.com/todos/1" as Todo
```
