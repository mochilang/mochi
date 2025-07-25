// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    fn countDivisors(n: i32) -> i32 {
        if n < 2 {
            return 1;
        }
        let mut count = 2;
        let mut i = 2;
        while (i <= n as f64) / (2 as f64) {
            if n % i == 0 {
                count += 1;
            }
            i += 1;
        }
        return count;
    }
    fn main() -> () {
        println!("The first 20 anti-primes are:");
        let mut maxDiv = 0;
        let mut count = 0;
        let mut n = 1;
        let mut line = String::new();
        while count < 20 {
            let d = countDivisors(n);
            if d > maxDiv {
                line = format!("{}{}", format!("{}{}", line, n.to_string()), " ");
                maxDiv = d;
                count += 1;
            }
            n += 1;
        }
        line = &line[0 as usize..line.len() as i32 - 1 as usize];
        println!("{}", vec![format!("{}", line)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
    }
    main();
}
