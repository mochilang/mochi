use std::collections::{HashMap, HashSet};
use std::io::{self, Read};

fn ladders(begin: String, end: String, words: Vec<String>) -> Vec<Vec<String>> {
    let word_set: HashSet<String> = words.into_iter().collect();
    if !word_set.contains(&end) { return vec![]; }
    let mut parents: HashMap<String, Vec<String>> = HashMap::new();
    let mut level: HashSet<String> = vec![begin.clone()].into_iter().collect();
    let mut visited: HashSet<String> = vec![begin.clone()].into_iter().collect();
    let mut found = false;
    while !level.is_empty() && !found {
        let mut next = HashSet::new();
        let mut cur: Vec<String> = level.iter().cloned().collect();
        cur.sort();
        for word in cur {
            let mut chars: Vec<char> = word.chars().collect();
            for i in 0..chars.len() {
                let orig = chars[i];
                for c in 'a'..='z' {
                    if c == orig { continue; }
                    chars[i] = c;
                    let nw: String = chars.iter().collect();
                    if !word_set.contains(&nw) || visited.contains(&nw) { continue; }
                    next.insert(nw.clone());
                    parents.entry(nw.clone()).or_default().push(word.clone());
                    if nw == end { found = true; }
                }
                chars[i] = orig;
            }
        }
        visited.extend(next.iter().cloned());
        level = next;
    }
    if !found { return vec![]; }
    fn backtrack(word: &str, begin: &str, parents: &HashMap<String, Vec<String>>, path: &mut Vec<String>, out: &mut Vec<Vec<String>>) {
        if word == begin {
            let mut seq = path.clone();
            seq.reverse();
            out.push(seq);
            return;
        }
        let mut plist = parents.get(word).cloned().unwrap_or_default();
        plist.sort();
        for p in plist {
            path.push(p.clone());
            backtrack(&p, begin, parents, path, out);
            path.pop();
        }
    }
    let mut out = vec![];
    let mut path = vec![end.clone()];
    backtrack(&end, &begin, &parents, &mut path, &mut out);
    out.sort_by_key(|v| v.join("->"));
    out
}

fn fmt(paths: Vec<Vec<String>>) -> String {
    let mut lines = vec![paths.len().to_string()];
    for p in paths { lines.push(p.join("->")); }
    lines.join("\n")
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() { return; }
    let tc: usize = lines[0].trim().parse().unwrap_or(0);
    let mut idx = 1usize;
    let mut out = Vec::new();
    for _ in 0..tc {
        let begin = lines[idx].to_string(); idx += 1;
        let end = lines[idx].to_string(); idx += 1;
        let n: usize = lines[idx].trim().parse().unwrap_or(0); idx += 1;
        let mut words = Vec::new();
        for _ in 0..n { words.push(lines[idx].to_string()); idx += 1; }
        out.push(fmt(ladders(begin, end, words)));
    }
    print!("{}", out.join("\n\n"));
}
