type Customer = {
  id: number;
  name: string;
};

type Order = {
  id: number;
  customerId: number;
  total: number;
};

type Result = {
  orderId: number;
  orderCustomerId: number;
  pairedCustomerName: string;
  orderTotal: number;
};

const customers: Customer[] = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
  { id: 3, name: "Charlie" }
];

const orders: Order[] = [
  { id: 100, customerId: 1, total: 250 },
  { id: 101, customerId: 2, total: 125 },
  { id: 102, customerId: 1, total: 300 }
];

const result: Result[] = [];

for (const o of orders) {
  for (const c of customers) {
    result.push({
      orderId: o.id,
      orderCustomerId: o.customerId,
      pairedCustomerName: c.name,
      orderTotal: o.total,
    });
  }
}

console.log("--- Cross Join: All order-customer pairs ---");
for (const entry of result) {
  console.log(`Order ${entry.orderId} (customerId: ${entry.orderCustomerId}, total: $${entry.orderTotal}) paired with ${entry.pairedCustomerName}`);
}
