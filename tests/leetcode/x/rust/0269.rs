use std::cmp::Reverse;
use std::collections::{BinaryHeap, BTreeMap, BTreeSet};
use std::io::{self, Read};

fn solve(words: &[String]) -> String {
    let mut chars = BTreeSet::new();
    for w in words {
        for c in w.chars() {
            chars.insert(c);
        }
    }
    let mut adj: BTreeMap<char, BTreeSet<char>> = BTreeMap::new();
    let mut indeg: BTreeMap<char, i32> = BTreeMap::new();
    for &c in &chars {
        adj.insert(c, BTreeSet::new());
        indeg.insert(c, 0);
    }
    for pair in words.windows(2) {
        let a = &pair[0];
        let b = &pair[1];
        let m = a.len().min(b.len());
        if a[..m] == b[..m] && a.len() > b.len() {
            return String::new();
        }
        for (x, y) in a.chars().zip(b.chars()) {
            if x != y {
                if adj.get_mut(&x).unwrap().insert(y) {
                    *indeg.get_mut(&y).unwrap() += 1;
                }
                break;
            }
        }
    }
    let mut heap: BinaryHeap<Reverse<char>> = BinaryHeap::new();
    for (&c, &d) in &indeg {
        if d == 0 {
            heap.push(Reverse(c));
        }
    }
    let mut out = String::new();
    while let Some(Reverse(c)) = heap.pop() {
        out.push(c);
        for &nei in adj.get(&c).unwrap() {
            let entry = indeg.get_mut(&nei).unwrap();
            *entry -= 1;
            if *entry == 0 {
                heap.push(Reverse(nei));
            }
        }
    }
    if out.chars().count() == chars.len() { out } else { String::new() }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return;
    }
    let t: usize = lines[0].trim().parse().unwrap();
    let mut idx = 1;
    let mut out = Vec::with_capacity(t);
    for _ in 0..t {
        let n: usize = lines[idx].trim().parse().unwrap();
        idx += 1;
        let words = lines[idx..idx + n].iter().map(|s| s.trim().to_string()).collect::<Vec<_>>();
        idx += n;
        out.push(solve(&words));
    }
    print!("{}", out.join("\n"));
}
