// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
fn main() {
    fn add(a: i32, b: i32) -> i32 {
        return a + b;
    }
    let add5 = |tmp1| add(5, tmp1);
    println!("{}", vec![format!("{}", add5(3))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
