// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    let target = 269696;
    let modulus = 1000000;
    let mut n = 1;
    while true {
        let square = n * n;
        let ending = square % modulus;
        if ending == target {
            println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", format!("{}{}", "The smallest number whose square ends with ", target.to_string()), " is "), n.to_string()))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
            break;
        }
        n += 1;
    }
}
