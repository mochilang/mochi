struct Person { name: &'static str, age: i32 }
struct Book { title: &'static str, author: Person }

fn main() {
    let book = Book {
        title: "Go",
        author: Person { name: "Bob", age: 42 },
    };
    println!("{}", book.author.name);
}
