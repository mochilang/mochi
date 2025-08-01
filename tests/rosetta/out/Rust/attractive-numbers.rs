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
    fn countPrimeFactors(n: &mut i32) -> i32 {
        if n == 1 {
            return 0;
        }
        if isPrime(n) {
            return 1;
        }
        let mut count = 0;
        let mut f = 2;
        while true {
            if n % f == 0 {
                count += 1;
                n /= f;
                if n == 1 {
                    return count;
                }
                if isPrime(n) {
                    f = n;
                }
            }
        }
        return count;
    }
    fn pad4(n: i32) -> &'static str {
        let mut s = n.to_string();
        while s.len() as i32 < 4 {
            s = format!("{}{}", " ", s);
        }
        return s;
    }
    fn main() -> () {
        let max = 120;
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", "The attractive numbers up to and including ", max.to_string()), " are:"))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        let mut count = 0;
        let mut line = String::new();
        let mut lineCount = 0;
        let mut i = 1;
        while i <= max {
            let c = countPrimeFactors(&mut i);
            if isPrime(c) {
                line += pad4(i);
                count += 1;
                lineCount += 1;
                if lineCount == 20 {
                    println!("{}", vec![format!("{}", line)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
                    line = "";
                    lineCount = 0;
                }
            }
            i += 1;
        }
        if lineCount > 0 {
            println!("{}", vec![format!("{}", line)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        }
    }
    main();
}
