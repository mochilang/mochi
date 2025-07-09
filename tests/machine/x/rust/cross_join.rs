fn main() {
    let customers = vec![{ let mut m = std::collections::HashMap::new(); m.insert("id", 1); m.insert("name", "Alice"); m }, { let mut m = std::collections::HashMap::new(); m.insert("id", 2); m.insert("name", "Bob"); m }, { let mut m = std::collections::HashMap::new(); m.insert("id", 3); m.insert("name", "Charlie"); m }];
    let orders = vec![{ let mut m = std::collections::HashMap::new(); m.insert("id", 100); m.insert("customerId", 1); m.insert("total", 250); m }, { let mut m = std::collections::HashMap::new(); m.insert("id", 101); m.insert("customerId", 2); m.insert("total", 125); m }, { let mut m = std::collections::HashMap::new(); m.insert("id", 102); m.insert("customerId", 1); m.insert("total", 300); m }];
    let result = { let mut tmp1 = Vec::new();for &o in &orders { for &c in &customers { tmp1.push({ let mut m = std::collections::HashMap::new(); m.insert("orderId", o.id); m.insert("orderCustomerId", o.customerId); m.insert("pairedCustomerName", c.name); m.insert("orderTotal", o.total); m }); } } tmp1 };
    println!("{:?}", "--- Cross Join: All order-customer pairs ---");
    for entry in result {
        println!("{:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?}", "Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName);
    }
}
