// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    let PI = 3.141592653589793;
    fn sinApprox(x: f64) -> f64 {
        let mut term = x;
        let mut sum = x;
        let mut n = 1;
        while n <= 10 {
            let denom = ((2 * n) * (2 * n + 1)).parse::<f64>().unwrap();
            term = -term * x * x / denom;
            sum += term;
            n += 1;
        }
        return sum;
    }
    fn cosApprox(x: f64) -> f64 {
        let mut term = 1.0;
        let mut sum = 1.0;
        let mut n = 1;
        while n <= 10 {
            let denom = ((2 * n - 1) * (2 * n)).parse::<f64>().unwrap();
            term = -term * x * x / denom;
            sum += term;
            n += 1;
        }
        return sum;
    }
    fn sqrtApprox(x: f64) -> f64 {
        let mut guess = x;
        let mut i = 0;
        while i < 10 {
            guess = (guess + x / guess) / 2.0;
            i += 1;
        }
        return guess;
    }
    let L = 10.0;
    let G = 9.81;
    let dt = 0.2;
    let phi0 = PI / 4.0;
    let omega = sqrtApprox(G / L);
    let mut t = 0.0;
    for step in 0..10 {
        let phi = phi0 * cosApprox(omega * t);
        let pos = (10.0 * sinApprox(phi) + 0.5).parse::<i32>().unwrap();
        println!("{}", vec![format!("{}", pos.to_string())].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        t += dt;
    }
}
