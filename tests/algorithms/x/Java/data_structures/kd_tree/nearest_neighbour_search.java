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

    static class SearchResult {
        double[] point;
        double distance;
        int nodes_visited;
        SearchResult(double[] point, double distance, int nodes_visited) {
            this.point = point;
            this.distance = distance;
            this.nodes_visited = nodes_visited;
        }
        SearchResult() {}
        @Override public String toString() {
            return String.format("{'point': %s, 'distance': %s, 'nodes_visited': %s}", String.valueOf(point), String.valueOf(distance), String.valueOf(nodes_visited));
        }
    }

    static KDNode[] nodes;
    static double[][] queries;
    static int q = 0;

    static double square_distance(double[] a, double[] b) {
        int i = 0;
        double total = 0.0;
        while (i < a.length) {
            double diff = a[i] - b[i];
            total = total + diff * diff;
            i = i + 1;
        }
        return total;
    }

    static SearchResult search(KDNode[] nodes, int index, double[] query_point, int depth, SearchResult best) {
        if (index == (-1)) {
            return best;
        }
        SearchResult result = best;
result.nodes_visited = result.nodes_visited + 1;
        KDNode node = nodes[index];
        double[] current_point = ((double[])(node.point));
        double current_dist = square_distance(((double[])(query_point)), ((double[])(current_point)));
        if (result.point.length == 0 || current_dist < result.distance) {
result.point = current_point;
result.distance = current_dist;
        }
        int k = query_point.length;
        int axis = Math.floorMod(depth, k);
        int nearer = node.left;
        int further = node.right;
        if (query_point[axis] > current_point[axis]) {
            nearer = node.right;
            further = node.left;
        }
        result = search(((KDNode[])(nodes)), nearer, ((double[])(query_point)), depth + 1, result);
        double diff_1 = query_point[axis] - current_point[axis];
        if (diff_1 * diff_1 < result.distance) {
            result = search(((KDNode[])(nodes)), further, ((double[])(query_point)), depth + 1, result);
        }
        return result;
    }

    static SearchResult nearest_neighbour_search(KDNode[] nodes, int root, double[] query_point) {
        SearchResult initial = new SearchResult(new double[]{}, 1000000000000000019884624838656.0, 0);
        return search(((KDNode[])(nodes)), root, ((double[])(query_point)), 0, initial);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            nodes = ((KDNode[])(new KDNode[]{new KDNode(new double[]{9.0, 1.0}, 1, 4), new KDNode(new double[]{2.0, 7.0}, 2, 3), new KDNode(new double[]{3.0, 6.0}, -1, -1), new KDNode(new double[]{6.0, 12.0}, -1, -1), new KDNode(new double[]{17.0, 15.0}, 5, 6), new KDNode(new double[]{13.0, 15.0}, -1, -1), new KDNode(new double[]{10.0, 19.0}, -1, -1)}));
            queries = ((double[][])(new double[][]{new double[]{9.0, 2.0}, new double[]{12.0, 15.0}, new double[]{1.0, 3.0}}));
            q = 0;
            while (q < queries.length) {
                SearchResult res = nearest_neighbour_search(((KDNode[])(nodes)), 0, ((double[])(queries[q])));
                System.out.println(_p(res.point) + " " + _p(res.distance) + " " + _p(res.nodes_visited) + "\n");
                q = q + 1;
            }
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
