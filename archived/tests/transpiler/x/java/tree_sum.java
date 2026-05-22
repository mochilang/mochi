public class Main {
    static Node t = new Node(Leaf, 1, new Node(Leaf, 2, Leaf));

    static int sum_tree(Tree t) {
        return t == Leaf ? 0 : t == Node(left, value, right) ? sum_tree(left) + value + sum_tree(right) : sum_tree(left) + value + sum_tree(right);
    }
    public static void main(String[] args) {
        System.out.println(sum_tree(t));
    }
}
