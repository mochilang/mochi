fn append(mut vec: Vec<i32>, value: i32) -> Vec<i32> {
    vec.push(value);
    vec
}

fn main() {
    let a = vec![1, 2];
    println!("{:?}", append(a, 3));
}
