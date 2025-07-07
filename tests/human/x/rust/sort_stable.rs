#[derive(Clone)]
struct Item { n: i32, v: &'static str }

fn main() {
    let mut items = vec![
        Item { n: 1, v: "a" },
        Item { n: 1, v: "b" },
        Item { n: 2, v: "c" },
    ];
    items.sort_by(|a, b| a.n.cmp(&b.n));
    let result: Vec<&str> = items.iter().map(|i| i.v).collect();
    println!("{:?}", result);
}
