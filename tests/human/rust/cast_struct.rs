struct Todo {
    title: String,
}

fn main() {
    let todo = Todo { title: "hi".to_string() };
    println!("{}", todo.title);
}
