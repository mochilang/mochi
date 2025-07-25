from __future__ import annotations
import dataclasses

@dataclasses.dataclass
class Customer:
    id: int
    name: str

@dataclasses.dataclass
class Order:
    id: int
    customerId: int
    total: int

@dataclasses.dataclass
class Result:
    orderId: int
    orderCustomerId: int
    pairedCustomerName: str
    orderTotal: int

customers = [
    Customer(id=1, name="Alice"),
    Customer(id=2, name="Bob"),
    Customer(id=3, name="Charlie"),
]
orders = [
    Order(id=100, customerId=1, total=250),
    Order(id=101, customerId=2, total=125),
    Order(id=102, customerId=1, total=300),
]
result = [
    Result(
        orderId=o.id,
        orderCustomerId=o.customerId,
        pairedCustomerName=c.name,
        orderTotal=o.total,
    )
    for o in orders
    for c in customers
]
print("--- Cross Join: All order-customer pairs ---")
for entry in result:
    print(
        f"Order {entry.orderId} (customerId: {entry.orderCustomerId}, total: ${entry.orderTotal}) "
        f"paired with {entry.pairedCustomerName}"
    )
