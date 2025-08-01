// Generated by Mochi transpiler v0.10.34 on 2025-07-21 23:34 +0700
#[derive(Debug, Clone)]
struct PeopleItem {
    name: String,
    age: i64,
}
impl std::fmt::Display for PeopleItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"name\": \"{}\"", self.name)?;
        write!(f, ", ")?;
        write!(f, "\"age\": {}", self.age)?;
        write!(f, "}}")
    }
}

fn main() {
    let people: Vec<PeopleItem> = vec![PeopleItem {name: String::from("Alice"), age: 30}, PeopleItem {name: String::from("Bob"), age: 25}];
    for _row in &people {
        println!("{}", _row);
    }
}
