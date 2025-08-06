public class Main {
    static class KDNode {
        double[] point;
        int left;
        int right;
        KDNode(double[] point, int left, int right) {
            this.point = point;
            this.left = left;
            this.right = right;
        }
        KDNode() {}
        @Override public String toString() {
            return String.format("{'point': %s, 'left': %s, 'right': %s}", String.valueOf(point), String.valueOf(left), String.valueOf(right));
        }
    }

    static KDNode[] nodes = new KDNode[0];
    static KDNode root;
    static KDNode left_child;
    static KDNode right_child;

    static KDNode make_kd_node(double[] point, int left, int right) {
        return new KDNode(point, left, right);
    }
    public static void main(String[] args) {
        nodes = ((KDNode[])(new KDNode[]{}));
        nodes = ((KDNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(make_kd_node(((double[])(new double[]{2.0, 3.0})), 1, 2))).toArray(KDNode[]::new)));
        nodes = ((KDNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(make_kd_node(((double[])(new double[]{1.0, 5.0})), -1, -1))).toArray(KDNode[]::new)));
        nodes = ((KDNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(make_kd_node(((double[])(new double[]{4.0, 2.0})), -1, -1))).toArray(KDNode[]::new)));
        root = nodes[0];
        left_child = nodes[1];
        right_child = nodes[2];
        System.out.println(_p(root.point));
        System.out.println(_p(root.left));
        System.out.println(_p(root.right));
        System.out.println(_p(left_child.point));
        System.out.println(_p(right_child.point));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
