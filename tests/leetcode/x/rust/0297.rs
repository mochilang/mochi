use std::cell::RefCell;
use std::collections::VecDeque;
use std::io::{self, Read};
use std::rc::Rc;

type Node = Option<Rc<RefCell<TreeNode>>>;

#[derive(Debug)]
struct TreeNode {
    val: i32,
    left: Node,
    right: Node,
}

struct Codec;

impl Codec {
    fn serialize(&self, root: Node) -> String {
        if root.is_none() {
            return "[]".to_string();
        }
        let mut out: Vec<String> = Vec::new();
        let mut q: VecDeque<Node> = VecDeque::new();
        q.push_back(root);
        while let Some(node) = q.pop_front() {
            if let Some(rc) = node {
                let node_ref = rc.borrow();
                out.push(node_ref.val.to_string());
                q.push_back(node_ref.left.clone());
                q.push_back(node_ref.right.clone());
            } else {
                out.push("null".to_string());
            }
        }
        while out.last().map(|s| s == "null").unwrap_or(false) {
            out.pop();
        }
        format!("[{}]", out.join(","))
    }

    fn deserialize(&self, data: &str) -> Node {
        if data == "[]" {
            return None;
        }
        let vals: Vec<&str> = data[1..data.len() - 1].split(',').collect();
        let root = Rc::new(RefCell::new(TreeNode { val: vals[0].parse().unwrap(), left: None, right: None }));
        let mut q: VecDeque<Rc<RefCell<TreeNode>>> = VecDeque::new();
        q.push_back(root.clone());
        let mut i = 1usize;
        while let Some(node) = q.pop_front() {
            if i < vals.len() && vals[i] != "null" {
                let child = Rc::new(RefCell::new(TreeNode { val: vals[i].parse().unwrap(), left: None, right: None }));
                node.borrow_mut().left = Some(child.clone());
                q.push_back(child);
            }
            i += 1;
            if i < vals.len() && vals[i] != "null" {
                let child = Rc::new(RefCell::new(TreeNode { val: vals[i].parse().unwrap(), left: None, right: None }));
                node.borrow_mut().right = Some(child.clone());
                q.push_back(child);
            }
            i += 1;
        }
        Some(root)
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<&str> = input.lines().map(|s| s.trim()).filter(|s| !s.is_empty()).collect();
    if lines.is_empty() {
        return;
    }
    let t: usize = lines[0].parse().unwrap();
    let codec = Codec;
    let mut out = Vec::new();
    for tc in 0..t {
        let root = codec.deserialize(lines[tc + 1]);
        out.push(codec.serialize(root));
    }
    print!("{}", out.join("\n\n"));
}
