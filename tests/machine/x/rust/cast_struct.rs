#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Todo {
        title: &'static str,
}

fn main() {
    let todo = Todo { title: "hi" };
    println!("{}", todo.title);
}
