// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    fn f() -> Vec<i32> {
        return vec![0, 0.0];
    }
    fn g(a: i32, b: f64) -> i32 {
        return 0;
    }
    fn h(s: &'static str, nums: Vec<i32>) -> () {
    }
    fn main() -> () {
        let ab = f();
        let a = ab[0];
        let b = ab[1];
        let cb = f()[1];
        let d = g(a, cb);
        let e = g(d, b);
        let mut i = g(d, 2.0);
        let mut list: Vec<i32> = vec![];
        list = { let mut tmp = list.clone(); tmp.push(a); tmp };
        list = { let mut tmp = list.clone(); tmp.push(d); tmp };
        list = { let mut tmp = list.clone(); tmp.push(e); tmp };
        list = { let mut tmp = list.clone(); tmp.push(i); tmp };
        i = list.len() as i32;
    }
    main();
}
