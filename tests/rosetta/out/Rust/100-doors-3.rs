// Generated by Mochi compiler v0.10.30 on 2025-07-19T02:23:42Z
fn main() {
    let mut result = String::new();
    for i in 1..101 {
        let mut j = 1;
        while j * j < i {
            j += 1;
        }
        if j * j == i {
            result = format!("{}{}", result, String::from("O"));
        } else {
            result = format!("{}{}", result, String::from("-"));
        }
    }
    println!("{}", vec![format!("{}", result)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
