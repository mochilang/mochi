## Collections

Maps, sets and lists are built in.

```mochi
let user = {"name": "Ana", "age": 22}
let tags = {"a", "b", "c"}
let nums = [1, 2, 3]

print(user["name"])
print(tags)
print(nums[1])
```

Strings can be treated like read-only lists of characters. Use the index
operator to access individual characters, or iterate over them with a
`for` loop:

```mochi
let text = "hello"
print(text[1])

for ch in text {
  print(ch)
}
```
