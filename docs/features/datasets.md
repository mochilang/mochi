## Dataset Queries

`from` expressions treat lists as simple datasets. Combine optional `where`, `sort by`, `skip`, `take` and `select` clauses to shape the result. Use `load` to read a CSV or JSONL file into a typed list.

```mochi
type Person {
  name: string
  age: int
}

let people = load "people.csv" as Person

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

Datasets can be joined together to combine related records:

```mochi
let result = from o in orders
             join from c in customers on o.customerId == c.id
             select {
               orderId: o.id,
               customerName: c.name,
               total: o.total
             }
```
