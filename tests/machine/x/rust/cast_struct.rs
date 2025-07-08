fn main() {
    struct Todo {
        title: &'static str,
    }
    let todo = Todo { title: "hi" };
    println!("{:?}", todo.title);
}
