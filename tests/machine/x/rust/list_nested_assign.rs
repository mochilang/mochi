// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
fn main() {
    let mut matrix = vec![vec![1, 2], vec![3, 4]];
    matrix[1][0] = 5;
    println!("{}", vec![format!("{}", matrix[1][0])].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
