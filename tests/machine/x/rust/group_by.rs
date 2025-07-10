#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct People {
    name: &'static str,
    age: i32,
    city: &'static str,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Group {
    key: &'static str,
    items: Vec<People>,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Result {
    city: &'static str,
    count: i32,
    avg_age: f64,
}

fn avg(v: &[i32]) -> f64 {
    let sum: i32 = v.iter().sum();
    sum as f64 / v.len() as f64
}

fn main() {
    let people = vec![People { name: "Alice", age: 30, city: "Paris" }, People { name: "Bob", age: 15, city: "Hanoi" }, People { name: "Charlie", age: 65, city: "Paris" }, People { name: "Diana", age: 45, city: "Hanoi" }, People { name: "Eve", age: 70, city: "Paris" }, People { name: "Frank", age: 22, city: "Hanoi" }];
    let stats = { let mut tmp1 = std::collections::HashMap::new();for person in &people { let key = person.city; tmp1.entry(key).or_insert_with(Vec::new).push(person.clone()); } let mut tmp2 = Vec::<Group>::new(); for (k,v) in tmp1 { tmp2.push(Group { key: k, items: v }); } let mut result = Vec::new(); for g in tmp2 { result.push(Result { city: g.key, count: g.clone().items.len() as i32, avg_age: avg(&{ let mut tmp3 = Vec::new();for p in &g.clone().items { tmp3.push(p.age); } tmp3 }) }); } result };
    println!("{}", "--- People grouped by city ---");
    for s in stats {
        println!("{:?} {} {:?} {} {:?}", s.city, ": count =", s.count, ", avg_age =", s.avg_age);
    }
}
