## Collections

Maps, sets and lists are built in. Literals use familiar syntax and support type inference.

```mochi
let user = {"name": "Ana", "age": 22}
let tags = {"a", "b", "c"}
let nums = [1, 2, 3]

print(user["name"])
print(tags)
print(nums[1])
```

Lists preserve insertion order and can be appended using `push` or concatenated with `+`.

```mochi
let names = ["a", "b"]
names.push("c")
print(names)
```

Maps and sets support membership tests via the `in` operator. Iterate over keys or elements with a `for` loop.

```mochi
if "a" in tags {
  print("tag found")
}

for k in user.keys() {
  print(k, user[k])
}
```

Strings can be treated like read-only lists of characters. Use the index operator to access individual characters, or iterate over them with a `for` loop:

```mochi
let text = "hello"
print(text[1])

for ch in text {
  print(ch)
}
```
