// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    fn ord(ch: &'static str) -> i32 {
        if ch == "a" {
            return 97;
        }
        if ch == "π" {
            return 960;
        }
        if ch == "A" {
            return 65;
        }
        return 0;
    }
    println!("{}", vec![format!("{}", ord("a").to_string())].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    println!("{}", vec![format!("{}", ord("π").to_string())].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
