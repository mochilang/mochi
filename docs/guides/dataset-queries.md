# Dataset Queries

Mochi treats lists as in-memory datasets. The `from` expression provides a concise way to filter and transform these collections. Use `load` to read data from CSV or JSONL files.

## Selecting Data

```mochi
type Person {
  name: string
  age: int
}

let people = load "people.csv" as Person

let adults = from p in people
             where p.age >= 18
             select {
               name: p.name,
               age: p.age
             }
```

## Sorting and Limiting

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
```

## Joining Lists

```mochi
let result = from o in orders
             join from c in customers on o.customerId == c.id
             select {
               orderId: o.id,
               customerName: c.name,
               total: o.total
             }
```
