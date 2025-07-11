fn main() {
    mod math {
        pub const pi: f64 = std::f64::consts::PI;
        pub const e: f64 = std::f64::consts::E;
        pub fn sqrt(x: f64) -> f64 { x.sqrt() }
        pub fn pow(x: f64, y: f64) -> f64 { x.powf(y) }
        pub fn sin(x: f64) -> f64 { x.sin() }
        pub fn log(x: f64) -> f64 { x.ln() }
    }
    let r = 3.0;
    let area = math::pi * math::pow(r, 2.0);
    let root = math::sqrt(49.0);
    let sin45 = math::sin(math::pi / 4.0);
    let log_e = math::log(math::e);
    println!("{} {} {} {}", "Circle area with r =", r, "=>", area);
    println!("{} {}", "Square root of 49:", root);
    println!("{} {}", "sin(π/4):", sin45);
    println!("{} {}", "log(e):", log_e);
}
