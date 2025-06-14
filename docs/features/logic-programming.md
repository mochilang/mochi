## Logic Programming

Mochi includes a tiny Datalog-style engine for expressing and querying facts.
Facts and rules are declared with the `fact` and `rule` keywords and queried using `query`.

```mochi
fact parent("Alice", "Bob")
fact parent("Alice", "Carol")
fact parent("Bob", "David")
fact parent("Carol", "Eva")

rule grandparent(x, z):-
  parent(x, y), parent(y, z)

rule sibling(x, y):-
  parent(p, x), parent(p, y), x != y

let grandparents = query grandparent(x, z)
print("Grandparents:")
for g in grandparents {
  print(g.x, "is grandparent of", g.z)
}

let siblings = query sibling(x, y)
print("Siblings:")
for s in siblings {
  print(s.x, "<->", s.y)
}
```

Queries return a list of objects keyed by variable name.
