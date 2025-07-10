#[derive(Default, Debug, Clone, PartialEq)]
struct Person {
        name: &'static str,
        age: i32,
        status: &'static str,
}

fn main() {
    let people: Vec<Person> = vec![Person { name: "Alice", age: 17, status: "minor" }, Person { name: "Bob", age: 25, status: "unknown" }, Person { name: "Charlie", age: 18, status: "unknown" }, Person { name: "Diana", age: 16, status: "minor" }];
    for tmp1 in 0..people.len() {
        let mut tmp2 = people[tmp1].clone();
        let mut name = tmp2.name;
        let mut age = tmp2.age;
        let mut status = tmp2.status;
        if age >= 18 {
            tmp2.status = "adult";
            tmp2.age = age + 1;
        }
        people[tmp1] = tmp2;
    }
    assert!(people == vec![Person { name: "Alice", age: 17, status: "minor" }, Person { name: "Bob", age: 26, status: "adult" }, Person { name: "Charlie", age: 19, status: "adult" }, Person { name: "Diana", age: 16, status: "minor" }]);
    println!("{:?}", "ok");
}
