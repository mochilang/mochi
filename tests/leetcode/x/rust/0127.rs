use std::collections::HashSet;
use std::io::{self, Read};

fn ladder_length(begin: String, end: String, words: Vec<String>) -> i32 {
    let word_set: HashSet<String> = words.into_iter().collect();
    if !word_set.contains(&end) {
        return 0;
    }
    let mut level: HashSet<String> = vec![begin.clone()].into_iter().collect();
    let mut visited: HashSet<String> = vec![begin].into_iter().collect();
    let mut steps = 1i32;
    while !level.is_empty() {
        if level.contains(&end) {
            return steps;
        }
        let mut next = HashSet::new();
        let mut cur: Vec<String> = level.iter().cloned().collect();
        cur.sort();
        for word in cur {
            let mut chars: Vec<char> = word.chars().collect();
            for i in 0..chars.len() {
                let orig = chars[i];
                for c in 'a'..='z' {
                    if c == orig {
                        continue;
                    }
                    chars[i] = c;
                    let nw: String = chars.iter().collect();
                    if word_set.contains(&nw) && !visited.contains(&nw) {
                        next.insert(nw);
                    }
                }
                chars[i] = orig;
            }
        }
        visited.extend(next.iter().cloned());
        level = next;
        steps += 1;
    }
    0
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return;
    }
    let tc: usize = lines[0].trim().parse().unwrap_or(0);
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..tc {
        let begin = lines[idx].to_string();
        idx += 1;
        let end = lines[idx].to_string();
        idx += 1;
        let n: usize = lines[idx].trim().parse().unwrap_or(0);
        idx += 1;
        let mut words = Vec::new();
        for _ in 0..n {
            words.push(lines[idx].to_string());
            idx += 1;
        }
        out.push(ladder_length(begin, end, words).to_string());
    }
    print!("{}", out.join("\n\n"));
}
