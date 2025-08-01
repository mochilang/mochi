// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
fn main() {
    mod math {
        pub const pi: f64 = std::f64::consts::PI;
        pub const e: f64 = std::f64::consts::E;
        pub fn sqrt(x: f64) -> f64 { x.sqrt() }
        pub fn pow(x: f64, y: f64) -> f64 { x.powf(y) }
        pub fn sin(x: f64) -> f64 { x.sin() }
        pub fn log(x: f64) -> f64 { x.ln() }
    }
    println!("{}", vec![format!("{}", math::sqrt(16.0))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    println!("{}", vec![format!("{}", math::pi)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
}
