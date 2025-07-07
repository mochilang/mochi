const customers = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
  { id: 3, name: "Charlie" },
  { id: 4, name: "Diana" }, // Has no order
];
const orders = [
  { id: 100, customerId: 1, total: 250 },
  { id: 101, customerId: 2, total: 125 },
  { id: 102, customerId: 1, total: 300 },
  { id: 103, customerId: 5, total: 80 }, // Unknown customer
];
interface Row {
  order: { id: number; customerId: number; total: number } | undefined;
  customer: { id: number; name: string } | undefined;
}
const result: Row[] = [];
for (const o of orders) {
  const c = customers.find((cu) => cu.id === o.customerId);
  result.push({ order: o, customer: c });
}
for (const c of customers) {
  if (!orders.find((o) => o.customerId === c.id)) {
    result.push({ order: undefined, customer: c });
  }
}
console.log("--- Outer Join using syntax ---");
for (const row of result) {
  if (row.order) {
    if (row.customer) {
      console.log(
        "Order",
        row.order.id,
        "by",
        row.customer.name,
        "- $",
        row.order.total
      );
    } else {
      console.log(
        "Order",
        row.order.id,
        "by",
        "Unknown",
        "- $",
        row.order.total
      );
    }
  } else if (row.customer) {
    console.log("Customer", row.customer.name, "has no orders");
  }
}
