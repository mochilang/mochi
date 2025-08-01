// Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:06 +0700
use std::collections::HashMap;
#[derive(Clone)]
struct Group<K, V> {
    key: K,
    items: Vec<V>,
}
#[derive(Debug, Clone)]
struct PeopleItem {
    name: String,
    age: i64,
    city: String,
}
impl std::fmt::Display for PeopleItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"name\": \"{}\"", self.name)?;
        write!(f, ", ")?;
        write!(f, "\"age\": {}", self.age)?;
        write!(f, ", ")?;
        write!(f, "\"city\": \"{}\"", self.city)?;
        write!(f, "}}")
    }
}
#[derive(Debug, Clone)]
struct QueryItem {
    city: String,
    count: i64,
    avg_age: f64,
}
impl std::fmt::Display for QueryItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"city\": \"{}\"", self.city)?;
        write!(f, ", ")?;
        write!(f, "\"count\": {}", self.count)?;
        write!(f, ", ")?;
        write!(f, "\"avg_age\": {}", self.avg_age)?;
        write!(f, "}}")
    }
}
fn main() {
    let people: Vec<PeopleItem> = vec![PeopleItem {name: String::from("Alice"), age: 30, city: String::from("Paris")}, PeopleItem {name: String::from("Bob"), age: 15, city: String::from("Hanoi")}, PeopleItem {name: String::from("Charlie"), age: 65, city: String::from("Paris")}, PeopleItem {name: String::from("Diana"), age: 45, city: String::from("Hanoi")}, PeopleItem {name: String::from("Eve"), age: 70, city: String::from("Paris")}, PeopleItem {name: String::from("Frank"), age: 22, city: String::from("Hanoi")}];
    let stats: Vec<QueryItem> = {
        let mut _q: Vec<QueryItem> = Vec::new();
        let mut _groups: HashMap<String, Group<String, PeopleItem>> = HashMap::new();
        let mut _order: Vec<String> = Vec::new();
        for person in &people {
            let key = person.city.clone();
            let ks = format!("{:?}", &key);
            let e = _groups.entry(ks.clone()).or_insert_with(|| {
                _order.push(ks.clone());
                Group::<String, PeopleItem> {key: key.clone(), items: Vec::new()}
            });
            e.items.push(person.clone());
        }
        for ks in _order {
            let g = &_groups[&ks];
            _q.push(QueryItem {city: String::from(g.key.clone()), count: g.items.len() as i64, avg_age: {
                let tmp = {
                    let mut _q: Vec<i64> = Vec::new();
                    for p in g.items.clone() {
                        _q.push(p.age);
                    }
                    _q
                };
                tmp.iter().map(|x| *x as f64).sum::<f64>() / (tmp.len() as f64)
            }});
        }
        _q
    };
    println!("{}", "--- People grouped by city ---");
    for s in &stats {
        println!("{}", format!("{} {} {} {} {}", s.city, ": count =", s.count, ", avg_age =", s.avg_age).trim_end());
    }
}
