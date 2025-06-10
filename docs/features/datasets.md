## Dataset Queries

`from` expressions treat lists as simple datasets. Combine optional `where`, `sort by`, `skip`, `take` and `select` clauses to shape the result.

```mochi
let people = [
  { name: "Alice", age: 30 },
  { name: "Bob", age: 15 },
  { name: "Charlie", age: 65 },
  { name: "Diana", age: 45 }
]

let adults = from person in people
             where person.age >= 18
             select {
               name: person.name,
               age: person.age,
               is_senior: person.age >= 60
             }

for person in adults {
  print(person.name, "is", person.age,
        if person.is_senior { " (senior)" } else { "" })
}
```

Queries can also sort records and limit the results:

```mochi
let products = [
  { name: "Laptop", price: 1500 },
  { name: "Smartphone", price: 900 },
  { name: "Tablet", price: 600 },
  { name: "Monitor", price: 300 },
  { name: "Keyboard", price: 100 },
  { name: "Mouse", price: 50 },
  { name: "Headphones", price: 200 }
]

let top = from p in products
          sort by -p.price
          skip 1
          take 3
          select p

for item in top {
  print(item.name, "costs $", item.price)
}
```

You can join two lists on matching fields to combine related data:

```mochi
let customers = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" }
]

let orders = [
  { id: 100, customerId: 1, total: 250 },
  { id: 101, customerId: 2, total: 125 }
]

let summary = from o in orders
              join from c in customers on o.customerId == c.id
              select { id: o.id, name: c.name, total: o.total }

for row in summary {
  print(row.name, "ordered $", row.total)
}
```
