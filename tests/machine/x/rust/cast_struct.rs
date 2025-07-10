#[derive(Default, Debug, Clone, PartialEq)]
struct Todo {
        title: &'static str,
}

fn main() {
    let todo = Todo { title: "hi" };
    println!("{}", todo.title);
}
