// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
fn main() {
    let x = 2;
    let label = match x {1 => "one", 2 => "two", 3 => "three", _ => "unknown", };
    println!("{}", vec![format!("{}", label)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
