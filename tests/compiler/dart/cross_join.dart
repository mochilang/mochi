Map<String, Function> _structParsers = {};

class Customer {
  int id;
  String name;
  Customer({required this.id, required this.name});
  factory Customer.fromJson(Map<String,dynamic> m) {
    return Customer(id: m['id'] as int, name: m['name'] as String);
  }
}

class Order {
  int id;
  int customerId;
  int total;
  Order({required this.id, required this.customerId, required this.total});
  factory Order.fromJson(Map<String,dynamic> m) {
    return Order(id: m['id'] as int, customerId: m['customerId'] as int, total: m['total'] as int);
  }
}

class PairInfo {
  int orderId;
  int orderCustomerId;
  String pairedCustomerName;
  int orderTotal;
  PairInfo({required this.orderId, required this.orderCustomerId, required this.pairedCustomerName, required this.orderTotal});
  factory PairInfo.fromJson(Map<String,dynamic> m) {
    return PairInfo(orderId: m['orderId'] as int, orderCustomerId: m['orderCustomerId'] as int, pairedCustomerName: m['pairedCustomerName'] as String, orderTotal: m['orderTotal'] as int);
  }
}

List<Customer> customers = [Customer(id: 1, name: "Alice"), Customer(id: 2, name: "Bob"), Customer(id: 3, name: "Charlie")];

List<Order> orders = [Order(id: 100, customerId: 1, total: 250), Order(id: 101, customerId: 2, total: 125), Order(id: 102, customerId: 1, total: 300)];

List<PairInfo> result = (() {
  var _res = [];
  for (var o in orders) {
    for (var c in customers) {
      _res.add(PairInfo(orderId: o.id, orderCustomerId: o.customerId, pairedCustomerName: c.name, orderTotal: o.total));
    }
  }
  return _res;
})();

void main() {
  _structParsers['Customer'] = (m) => Customer.fromJson(m);
  _structParsers['Order'] = (m) => Order.fromJson(m);
  _structParsers['PairInfo'] = (m) => PairInfo.fromJson(m);
  
  print("--- Cross Join: All order-customer pairs ---");
  for (var entry in result) {
    print(["Order".toString(), entry.orderId.toString(), "(customerId:".toString(), entry.orderCustomerId.toString(), ", total: \\$".toString(), entry.orderTotal.toString(), ") paired with".toString(), entry.pairedCustomerName.toString()].join(' '));
  }
}
