// Generated by Mochi 0.10.34 on 2025-07-22 09:58 +0700
using System;
using System.Collections.Generic;
using System.Linq;
struct Customer {
    public int id;
    public string name;
    public override string ToString() => $"Customer {{id = {id}, name = {name}}}";
}
struct Order {
    public int id;
    public int customerId;
    public int total;
    public override string ToString() => $"Order {{id = {id}, customerId = {customerId}, total = {total}}}";
}
struct OResult {
    public string customerName;
    public Order order;
    public override string ToString() => $"OResult {{customerName = {customerName}, order = {order}}}";
}
class Program {
    static Customer[] customers = new Customer[]{new Customer{id = 1, name = "Alice"}, new Customer{id = 2, name = "Bob"}, new Customer{id = 3, name = "Charlie"}, new Customer{id = 4, name = "Diana"}};
    static Order[] orders = new Order[]{new Order{id = 100, customerId = 1, total = 250}, new Order{id = 101, customerId = 2, total = 125}, new Order{id = 102, customerId = 1, total = 300}};
    static OResult[] result = (from o in orders ccustomersc.ido.customerIdcTmp from c in cTmp.DefaultIfEmpty() select new OResult{customerName = c.name, order = o}).ToArray();
    static void Main() {
        Console.WriteLine("--- Right Join using syntax ---");
        foreach (var entry in result) {
            if (entry.order) {
                Console.WriteLine(("[" + (string.Join(", ", new object[]{"Customer", entry.customerName, "has order", entry.order.id, "- $", entry.order.total}) + "]")));
            }
        }
    }
}
