// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
fn main() {
    println!("{}", vec![format!("{}", if "a" < "b" {1} else {0})].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    println!("{}", vec![format!("{}", if "a" <= "a" {1} else {0})].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    println!("{}", vec![format!("{}", if "b" > "a" {1} else {0})].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    println!("{}", vec![format!("{}", if "b" >= "b" {1} else {0})].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
