public class Main {
    static class Node {
        java.math.BigInteger value;
        java.math.BigInteger left;
        java.math.BigInteger right;
        Node(java.math.BigInteger value, java.math.BigInteger left, java.math.BigInteger right) {
            this.value = value;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'left': %s, 'right': %s}", String.valueOf(value), String.valueOf(left), String.valueOf(right));
        }
    }

    static Node[] example;

    static java.math.BigInteger node_sum(Node[] tree, java.math.BigInteger index) {
        if (index.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            return java.math.BigInteger.valueOf(0);
        }
        Node node_1 = tree[(int)(((java.math.BigInteger)(index)).longValue())];
        return new java.math.BigInteger(String.valueOf(node_1.value.add(node_sum(((Node[])(tree)), new java.math.BigInteger(String.valueOf(node_1.left)))).add(node_sum(((Node[])(tree)), new java.math.BigInteger(String.valueOf(node_1.right))))));
    }
    public static void main(String[] args) {
        example = ((Node[])(new Node[]{new Node(java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2)), new Node(java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))), new Node(new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(3)).negate())), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5)), new Node(java.math.BigInteger.valueOf(12), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))), new Node(java.math.BigInteger.valueOf(8), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))), new Node(java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())))}));
        System.out.println(node_sum(((Node[])(example)), java.math.BigInteger.valueOf(0)));
    }
}
