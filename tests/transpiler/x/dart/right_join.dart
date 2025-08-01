// Generated by Mochi transpiler
class Customer {
  final int id;
  final String name;
  const Customer({required this.id, required this.name});
}

class Order {
  final int id;
  final int customerId;
  final int total;
  const Order({required this.id, required this.customerId, required this.total});
}

class Result {
  final String customerName;
  final Order order;
  const Result({required this.customerName, required this.order});
}

void main() {
  final List<Customer> customers = [Customer(id: 1, name: "Alice"), Customer(id: 2, name: "Bob"), Customer(id: 3, name: "Charlie"), Customer(id: 4, name: "Diana")];
  final List<Order> orders = [Order(id: 100, customerId: 1, total: 250), Order(id: 101, customerId: 2, total: 125), Order(id: 102, customerId: 1, total: 300)];
  final List<Result> result = (() {
  var results = [];
  for (var o in orders) {
    var matched = false;
    for (var c in customers) {
      if (!(o.customerId == c.id)) continue;
      matched = true;
      results.add(Result(customerName: c.name, order: o));
    }
    if (!matched) {
      var c = null;
      results.add(Result(customerName: c.name, order: o));
    }
  }
  return results;
})();
  print("--- Right Join using syntax ---");
  for (var entry in result) {
    if (entry.order) {
    print(["Customer", entry.customerName, "has order", entry.order.id, "- \$", entry.order.total].join(" "));
  } else {
    print(["Customer", entry.customerName, "has no orders"].join(" "));
  }
  }
}
