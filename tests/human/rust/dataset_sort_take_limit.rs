#[derive(Clone)]
struct Product { name: &'static str, price: i32 }

fn main() {
    let products = vec![
        Product { name: "Laptop", price: 1500 },
        Product { name: "Smartphone", price: 900 },
        Product { name: "Tablet", price: 600 },
        Product { name: "Monitor", price: 300 },
        Product { name: "Keyboard", price: 100 },
        Product { name: "Mouse", price: 50 },
        Product { name: "Headphones", price: 200 },
    ];

    let mut sorted = products.clone();
    sorted.sort_by(|a, b| b.price.cmp(&a.price));
    let selected: Vec<_> = sorted.into_iter().skip(1).take(3).collect();

    println!("--- Top products (excluding most expensive) ---");
    for item in selected {
        println!("{} costs $ {}", item.name, item.price);
    }
}
