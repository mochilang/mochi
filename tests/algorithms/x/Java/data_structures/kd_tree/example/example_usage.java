public class Main {
    static class Node {
        double[] point;
        int left;
        int right;
        Node(double[] point, int left, int right) {
            this.point = point;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'point': %s, 'left': %s, 'right': %s}", String.valueOf(point), String.valueOf(left), String.valueOf(right));
        }
    }

    static class BuildResult {
        int index;
        Node[] nodes;
        BuildResult(int index, Node[] nodes) {
            this.index = index;
            this.nodes = nodes;
        }
        BuildResult() {}
        @Override public String toString() {
            return String.format("{'index': %s, 'nodes': %s}", String.valueOf(index), String.valueOf(nodes));
        }
    }

    static int seed = 0;
    static class SearchResult {
        double[] point;
        double dist;
        int visited;
        SearchResult(double[] point, double dist, int visited) {
            this.point = point;
            this.dist = dist;
            this.visited = visited;
        }
        SearchResult() {}
        @Override public String toString() {
            return String.format("{'point': %s, 'dist': %s, 'visited': %s}", String.valueOf(point), String.valueOf(dist), String.valueOf(visited));
        }
    }


    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (1.0 * rand()) / 2147483648.0;
    }

    static double[][] hypercube_points(int num_points, double cube_size, int num_dimensions) {
        double[][] pts = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < num_points) {
            double[] p = ((double[])(new double[]{}));
            int j = 0;
            while (j < num_dimensions) {
                p = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(p), java.util.stream.DoubleStream.of(cube_size * random())).toArray()));
                j = j + 1;
            }
            pts = ((double[][])(appendObj((double[][])pts, p)));
            i = i + 1;
        }
        return pts;
    }

    static double[][] sort_points(double[][] points, int axis) {
        int n = points.length;
        int i_1 = 1;
        while (i_1 < n) {
            double[] key = ((double[])(points[i_1]));
            int j_1 = i_1 - 1;
            while (j_1 >= 0 && points[j_1][axis] > key[axis]) {
points[j_1 + 1] = ((double[])(points[j_1]));
                j_1 = j_1 - 1;
            }
points[j_1 + 1] = ((double[])(key));
            i_1 = i_1 + 1;
        }
        return points;
    }

    static double[][] sublist(double[][] arr, int start, int end) {
        double[][] res = ((double[][])(new double[][]{}));
        int i_2 = start;
        while (i_2 < end) {
            res = ((double[][])(appendObj((double[][])res, arr[i_2])));
            i_2 = i_2 + 1;
        }
        return res;
    }

    static Node[] shift_nodes(Node[] nodes, int offset) {
        int i_3 = 0;
        while (i_3 < nodes.length) {
            if (nodes[i_3].left != 0 - 1) {
            }
            if (nodes[i_3].right != 0 - 1) {
            }
            i_3 = i_3 + 1;
        }
        return nodes;
    }

    static BuildResult build_kdtree(double[][] points, int depth) {
        if (points.length == 0) {
            return new BuildResult(0 - 1, new Node[]{});
        }
        int k = points[0].length;
        int axis = Math.floorMod(depth, k);
        points = ((double[][])(sort_points(((double[][])(points)), axis)));
        int median = Math.floorDiv(points.length, 2);
        double[][] left_points = ((double[][])(sublist(((double[][])(points)), 0, median)));
        double[][] right_points = ((double[][])(sublist(((double[][])(points)), median + 1, points.length)));
        BuildResult left_res = build_kdtree(((double[][])(left_points)), depth + 1);
        BuildResult right_res = build_kdtree(((double[][])(right_points)), depth + 1);
        int offset = left_res.nodes.length + 1;
        Node[] shifted_right = ((Node[])(shift_nodes(((Node[])(right_res.nodes)), offset)));
        Node[] nodes = ((Node[])(left_res.nodes));
        int left_index = left_res.index;
        int right_index = right_res.index == 0 - 1 ? 0 - 1 : right_res.index + offset;
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new Node(points[median], left_index, right_index))).toArray(Node[]::new)));
        nodes = ((Node[])(concat(nodes, shifted_right)));
        int root_index = left_res.nodes.length;
        return new BuildResult(root_index, nodes);
    }

    static double square_distance(double[] a, double[] b) {
        double sum = 0.0;
        int i_4 = 0;
        while (i_4 < a.length) {
            double diff = a[i_4] - b[i_4];
            sum = sum + diff * diff;
            i_4 = i_4 + 1;
        }
        return sum;
    }

    static SearchResult nearest_neighbour_search(Node[] tree, int root, double[] query_point) {
        double[] nearest_point = ((double[])(new double[]{}));
        double nearest_dist = 0.0;
        int visited = 0;
        int i_5 = 0;
        while (i_5 < tree.length) {
            Node node = tree[i_5];
            double dist = square_distance(((double[])(query_point)), ((double[])(node.point)));
            visited = visited + 1;
            if (visited == 1 || dist < nearest_dist) {
                nearest_point = ((double[])(node.point));
                nearest_dist = dist;
            }
            i_5 = i_5 + 1;
        }
        return new SearchResult(nearest_point, nearest_dist, visited);
    }

    static String list_to_string(double[] arr) {
        String s = "[";
        int i_6 = 0;
        while (i_6 < arr.length) {
            s = s + _p(_getd(arr, i_6));
            if (i_6 < arr.length - 1) {
                s = s + ", ";
            }
            i_6 = i_6 + 1;
        }
        return s + "]";
    }

    static void main() {
        int num_points = 5000;
        double cube_size = 10.0;
        int num_dimensions = 10;
        double[][] pts_1 = ((double[][])(hypercube_points(num_points, cube_size, num_dimensions)));
        BuildResult build = build_kdtree(((double[][])(pts_1)), 0);
        int root = build.index;
        Node[] tree = ((Node[])(build.nodes));
        double[] query = ((double[])(new double[]{}));
        int i_7 = 0;
        while (i_7 < num_dimensions) {
            query = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(query), java.util.stream.DoubleStream.of(random())).toArray()));
            i_7 = i_7 + 1;
        }
        SearchResult res_1 = nearest_neighbour_search(((Node[])(tree)), root, ((double[])(query)));
        System.out.println("Query point: " + String.valueOf(list_to_string(((double[])(query)))));
        System.out.println("Nearest point: " + String.valueOf(list_to_string(((double[])(res_1.point)))));
        System.out.println("Distance: " + _p(res_1.dist));
        System.out.println("Nodes visited: " + _p(res_1.visited));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
            main();
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
