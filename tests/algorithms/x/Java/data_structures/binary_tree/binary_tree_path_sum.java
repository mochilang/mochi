public class Main {
    interface Tree {}

    static class Empty implements Tree {
        Empty() {
        }
        Empty() {}
        @Override public String toString() {
            return "Empty{}";
        }
    }

    static class Node implements Tree {
        Tree left;
        int value;
        Tree right;
        Node(Tree left, int value, Tree right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'left': %s, 'value': %s, 'right': %s}", String.valueOf(left), String.valueOf(value), String.valueOf(right));
        }
    }


    static int dfs(Tree node, int target, int current) {
        return node instanceof Empty ? 0 : (current + ((Number)(v)).intValue() == target ? 1 : 0) + dfs(((Node)(node)).left, target, current + ((Number)(((Node)(node)).value)).intValue()) + dfs(((Node)(node)).right, target, current + ((Number)(((Node)(node)).value)).intValue());
    }

    static int path_sum(Tree node, int target) {
        return node instanceof Empty ? 0 : dfs(node, target, 0) + path_sum(((Node)(node)).left, target) + path_sum(((Node)(node)).right, target);
    }

    static Tree sample_tree_one() {
        return new Node(new Node(new Node(new Node(Empty, 3, Empty), 3, new Node(Empty, -2, Empty)), 5, new Node(Empty, 2, new Node(Empty, 1, Empty))), 10, new Node(Empty, -3, new Node(Empty, 11, Empty)));
    }

    static Tree sample_tree_two() {
        return new Node(new Node(new Node(new Node(Empty, 3, Empty), 3, new Node(Empty, -2, Empty)), 5, new Node(Empty, 2, new Node(Empty, 1, Empty))), 10, new Node(Empty, -3, new Node(Empty, 10, Empty)));
    }

    static void main() {
        Tree tree1 = sample_tree_one();
        System.out.println(path_sum(tree1, 8));
        System.out.println(path_sum(tree1, 7));
        Tree tree2 = sample_tree_two();
        System.out.println(path_sum(tree2, 8));
    }
    public static void main(String[] args) {
        main();
    }
}
