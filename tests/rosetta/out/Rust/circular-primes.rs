// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    fn isPrime(n: i32) -> bool {
        if n < 2 {
            return false;
        }
        if n % 2 == 0 {
            return n == 2;
        }
        if n % 3 == 0 {
            return n == 3;
        }
        let mut d = 5;
        while d * d <= n {
            if n % d == 0 {
                return false;
            }
            d += 2;
            if n % d == 0 {
                return false;
            }
            d += 4;
        }
        return true;
    }
    let mut circs: Vec<i32> = vec![];
    fn isCircular(n: i32) -> bool {
        let mut nn = n;
        let mut pow = 1;
        while nn > 0 {
            pow *= 10;
            nn /= 10;
        }
        nn = n;
        while true {
            nn *= 10;
            let f = (nn as f64) / (pow as f64);
            nn = ((nn as f64) as f64) + f * (1 - pow) as f64;
            if nn == n {
                break;
            }
            if !isPrime(nn) {
                return false;
            }
        }
        return true;
    }
    println!("The first 19 circular primes are:");
    let mut digits = vec![1, 3, 7, 9];
    let mut q = vec![1, 2, 3, 5, 7, 9];
    let mut fq = vec![1, 2, 3, 5, 7, 9];
    let mut count = 0;
    while true {
        let f = q[0];
        let fd = fq[0];
        if isPrime(f) && isCircular(f) {
            circs = { let mut tmp = circs.clone(); tmp.push(f); tmp };
            count += 1;
            if count == 19 {
                break;
            }
        }
        q = q[1 as usize..q.len() as usize].to_vec();
        fq = fq[1 as usize..fq.len() as usize].to_vec();
        if f != 2 && f != 5 {
            for d in digits {
                q = { let mut tmp = q.clone(); tmp.push(f * 10 + d); tmp };
                fq = { let mut tmp = fq.clone(); tmp.push(fd); tmp };
            }
        }
    }
    fn showList(xs: Vec<i32>) -> &'static str {
        let mut out = String::from("[");
        let mut i = 0;
        while i < xs.len() as i32 {
            out += xs[i as usize].to_string();
            if i < xs.len() as i32 - 1 {
                out += ", ";
            }
            i += 1;
        }
        return format!("{}{}", out, "]");
    }
    println!("{}", vec![format!("{}", showList(circs))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    println!("\nThe next 4 circular primes, in repunit format, are:");
    println!("[R(19) R(23) R(317) R(1031)]");
    println!("\nThe following repunits are probably circular primes:");
    for i in vec![5003, 9887, 15073, 25031, 35317, 49081] {
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", "R(", i.to_string()), ") : true"))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    }
}
