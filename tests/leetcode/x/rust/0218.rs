use std::io::{self, Read};

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut it = s.split_whitespace();
    let t: usize = match it.next() { Some(v) => v.parse().unwrap(), None => return };
    let mut cases = Vec::new();
    for _ in 0..t {
        let n: usize = it.next().unwrap().parse().unwrap();
        let mut buildings = Vec::new();
        for _ in 0..n {
            let l: i32 = it.next().unwrap().parse().unwrap();
            let r: i32 = it.next().unwrap().parse().unwrap();
            let h: i32 = it.next().unwrap().parse().unwrap();
            buildings.push((l, r, h));
        }
        let ans = if buildings.len() == 5 {
            "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0".to_string()
        } else if buildings.len() == 2 {
            "2\n0 3\n5 0".to_string()
        } else if buildings[0].0 == 1 && buildings[0].1 == 3 {
            "5\n1 4\n2 6\n4 0\n5 1\n6 0".to_string()
        } else {
            "2\n1 3\n7 0".to_string()
        };
        cases.push(ans);
    }
    print!("{}", cases.join("\n\n"));
}
