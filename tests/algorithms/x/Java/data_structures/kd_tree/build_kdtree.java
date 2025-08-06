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

    static KDNode[] tree = new KDNode[0];
    static double[][] pts;
    static int root;

    static double[][] sort_points(double[][] points, int axis) {
        double[][] arr = ((double[][])(points));
        int i = 0;
        while (i < arr.length) {
            int j = 0;
            while (j < arr.length - 1) {
                if (arr[j][axis] > arr[j + 1][axis]) {
                    double[] tmp = ((double[])(arr[j]));
arr[j] = ((double[])(arr[j + 1]));
arr[j + 1] = ((double[])(tmp));
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static int build_kdtree(double[][] points, int depth) {
        if (points.length == 0) {
            return 0 - 1;
        }
        int k = points[0].length;
        int axis = Math.floorMod(depth, k);
        double[][] sorted = ((double[][])(sort_points(((double[][])(points)), axis)));
        int median_idx = sorted.length / 2;
        double[][] left_points = ((double[][])(java.util.Arrays.copyOfRange(sorted, 0, median_idx)));
        double[][] right_points = ((double[][])(java.util.Arrays.copyOfRange(sorted, median_idx + 1, sorted.length)));
        int idx = tree.length;
        tree = ((KDNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(tree), java.util.stream.Stream.of(new KDNode(sorted[median_idx], 0 - 1, 0 - 1))).toArray(KDNode[]::new)));
        int left_idx = build_kdtree(((double[][])(left_points)), depth + 1);
        int right_idx = build_kdtree(((double[][])(right_points)), depth + 1);
        KDNode node = tree[idx];
node.left = left_idx;
node.right = right_idx;
tree[idx] = node;
        return idx;
    }
    public static void main(String[] args) {
        tree = ((KDNode[])(new KDNode[]{}));
        pts = ((double[][])(new double[][]{new double[]{2.0, 3.0}, new double[]{5.0, 4.0}, new double[]{9.0, 6.0}, new double[]{4.0, 7.0}, new double[]{8.0, 1.0}, new double[]{7.0, 2.0}}));
        root = build_kdtree(((double[][])(pts)), 0);
        System.out.println(_p(tree));
        System.out.println(root);
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
