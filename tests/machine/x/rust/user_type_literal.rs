#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Person {
        name: &'static str,
        age: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Book {
        title: &'static str,
        author: Person,
}

fn main() {
    let book = Book { title: "Go", author: Person { name: "Bob", age: 42 } };
    println!("{}", book.author.name);
}
