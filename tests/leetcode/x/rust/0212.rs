use std::collections::HashMap;
use std::io::{self, Read};

#[derive(Default)]
struct Node {
    children: HashMap<u8, Node>,
    word: Option<String>,
}

fn solve(mut board: Vec<Vec<u8>>, words: Vec<String>) -> Vec<String> {
    let mut root = Node::default();
    for word in words {
        let mut node = &mut root;
        for &ch in word.as_bytes() {
            node = node.children.entry(ch).or_default();
        }
        node.word = Some(word);
    }
    let rows = board.len();
    let cols = board[0].len();
    let mut found = Vec::new();

    fn dfs(r: usize, c: usize, board: &mut Vec<Vec<u8>>, node: &mut Node, found: &mut Vec<String>) {
        let ch = board[r][c];
        let Some(next) = node.children.get_mut(&ch) else { return; };
        if let Some(word) = next.word.take() {
            found.push(word);
        }
        board[r][c] = b'#';
        if r > 0 && board[r - 1][c] != b'#' {
            dfs(r - 1, c, board, next, found);
        }
        if r + 1 < board.len() && board[r + 1][c] != b'#' {
            dfs(r + 1, c, board, next, found);
        }
        if c > 0 && board[r][c - 1] != b'#' {
            dfs(r, c - 1, board, next, found);
        }
        if c + 1 < board[0].len() && board[r][c + 1] != b'#' {
            dfs(r, c + 1, board, next, found);
        }
        board[r][c] = ch;
    }

    for r in 0..rows {
        for c in 0..cols {
            if root.children.contains_key(&board[r][c]) {
                dfs(r, c, &mut board, &mut root, &mut found);
            }
        }
    }
    found.sort();
    found
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut it = s.split_whitespace();
    let t: usize = match it.next() { Some(v) => v.parse().unwrap(), None => return };
    let mut cases = Vec::new();
    for _ in 0..t {
        let rows: usize = it.next().unwrap().parse().unwrap();
        let _cols: usize = it.next().unwrap().parse().unwrap();
        let mut board = Vec::new();
        for _ in 0..rows {
            board.push(it.next().unwrap().as_bytes().to_vec());
        }
        let n: usize = it.next().unwrap().parse().unwrap();
        let mut words = Vec::new();
        for _ in 0..n {
            words.push(it.next().unwrap().to_string());
        }
        let ans = solve(board, words);
        let mut lines = vec![ans.len().to_string()];
        lines.extend(ans);
        cases.push(lines.join("\n"));
    }
    print!("{}", cases.join("\n\n"));
}
