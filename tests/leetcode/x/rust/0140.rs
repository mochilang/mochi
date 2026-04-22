use std::collections::{HashMap, HashSet};
use std::io::{self, Read};

fn word_break(s: String, words: Vec<String>) -> Vec<String> {
    let word_set: HashSet<String> = words.iter().cloned().collect();
    let mut lengths: Vec<usize> = words.iter().map(|w| w.len()).collect();
    lengths.sort();
    lengths.dedup();
    let mut memo: HashMap<usize, Vec<String>> = HashMap::new();

    fn dfs(
        i: usize,
        s: &str,
        word_set: &HashSet<String>,
        lengths: &[usize],
        memo: &mut HashMap<usize, Vec<String>>,
    ) -> Vec<String> {
        if let Some(v) = memo.get(&i) {
            return v.clone();
        }
        let mut out = Vec::new();
        if i == s.len() {
            out.push(String::new());
        } else {
            for &length in lengths {
                let j = i + length;
                if j > s.len() {
                    break;
                }
                let word = &s[i..j];
                if word_set.contains(word) {
                    for tail in dfs(j, s, word_set, lengths, memo) {
                        if tail.is_empty() {
                            out.push(word.to_string());
                        } else {
                            out.push(format!("{word} {tail}"));
                        }
                    }
                }
            }
            out.sort();
        }
        memo.insert(i, out.clone());
        out
    }

    dfs(0, &s, &word_set, &lengths, &mut memo)
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
        let s = lines[idx].to_string();
        idx += 1;
        let n: usize = lines[idx].trim().parse().unwrap_or(0);
        idx += 1;
        let mut words = Vec::new();
        for _ in 0..n {
            words.push(lines[idx].to_string());
            idx += 1;
        }
        let ans = word_break(s, words);
        let mut block = vec![ans.len().to_string()];
        block.extend(ans);
        out.push(block.join("\n"));
    }
    print!("{}", out.join("\n\n"));
}
