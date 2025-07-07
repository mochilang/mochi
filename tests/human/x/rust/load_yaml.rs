use serde::Deserialize;
use std::fs;

#[derive(Deserialize, Debug)]
struct Person {
    name: String,
    age: i32,
    email: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let data = fs::read_to_string("../interpreter/valid/people.yaml")?;
    let people: Vec<Person> = serde_yaml::from_str(&data)?;
    let adults: Vec<_> = people.into_iter().filter(|p| p.age >= 18).collect();
    for a in adults {
        println!("{} {}", a.name, a.email);
    }
    Ok(())
}
