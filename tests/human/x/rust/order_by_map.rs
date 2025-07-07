#[derive(Debug)]
struct Record { a: i32, b: i32 }

fn main() {
    let mut data = vec![
        Record { a: 1, b: 2 },
        Record { a: 1, b: 1 },
        Record { a: 0, b: 5 },
    ];
    data.sort_by(|x, y| x.a.cmp(&y.a).then(x.b.cmp(&y.b)));
    println!("{:?}", data);
}
