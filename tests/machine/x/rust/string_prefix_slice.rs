fn main() {
    let prefix = "fore";
    let s1 = "forest";
    println!("{:?}", &s1[0 as usize..prefix.len() as usize] == prefix);
    let s2 = "desert";
    println!("{:?}", &s2[0 as usize..prefix.len() as usize] == prefix);
}
