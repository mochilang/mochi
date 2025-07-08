fn main() {
    let data = vec![1, 2];
    let flag = ({ let mut tmp1 = Vec::new();for x in &data { if !(x == 1) { continue; } tmp1.push(x); } tmp1 }.len() > 0);
    println!("{:?}", flag);
}
