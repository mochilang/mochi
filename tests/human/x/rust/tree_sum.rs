enum Tree {
    Leaf,
    Node { left: Box<Tree>, value: i32, right: Box<Tree> },
}

fn sum_tree(t: &Tree) -> i32 {
    match t {
        Tree::Leaf => 0,
        Tree::Node { left, value, right } => sum_tree(left) + value + sum_tree(right),
    }
}

fn main() {
    let t = Tree::Node {
        left: Box::new(Tree::Leaf),
        value: 1,
        right: Box::new(Tree::Node {
            left: Box::new(Tree::Leaf),
            value: 2,
            right: Box::new(Tree::Leaf),
        }),
    };
    println!("{}", sum_tree(&t));
}
