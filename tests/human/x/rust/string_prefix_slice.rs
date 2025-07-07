fn main() {
    let prefix = "fore";
    let s1 = "forest";
    println!("{}", &s1[0..prefix.len()] == prefix);
    let s2 = "desert";
    println!("{}", &s2[0..prefix.len()] == prefix);
}
