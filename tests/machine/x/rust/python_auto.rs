fn main() {
    mod math {
        pub const pi: f64 = std::f64::consts::PI;
        pub const e: f64 = std::f64::consts::E;
        pub fn sqrt(x: f64) -> f64 { x.sqrt() }
        pub fn pow(x: f64, y: f64) -> f64 { x.powf(y) }
        pub fn sin(x: f64) -> f64 { x.sin() }
        pub fn log(x: f64) -> f64 { x.ln() }
    }
    println!("{:?}", math::sqrt(16.0));
    println!("{:?}", math::pi);
}
