fn main() {
    let people = vec![{ let mut m = std::collections::HashMap::new(); m.insert("name", "Alice"); m.insert("age", 30); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Bob"); m.insert("age", 15); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Charlie"); m.insert("age", 65); m }, { let mut m = std::collections::HashMap::new(); m.insert("name", "Diana"); m.insert("age", 45); m }];
    let adults = { let mut tmp1 = Vec::new();for person in &people { if !(person.age >= 18) { continue; } tmp1.push({ let mut m = std::collections::HashMap::new(); m.insert("name", person.name); m.insert("age", person.age); m.insert("is_senior", person.age >= 60); m }); } tmp1 };
    println!("{:?}", "--- Adults ---");
    for person in adults {
        println!("{:?} {:?} {:?} {:?}", person.name, "is", person.age, if person.is_senior { " (senior)" } else { "" });
    }
}
