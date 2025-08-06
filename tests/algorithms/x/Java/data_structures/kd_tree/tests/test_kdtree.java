public class Main {
    static double INF;
    static int seed = 0;

    static double rand_float() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return (((Number)(seed)).doubleValue()) / 2147483648.0;
    }

    static double[][] hypercube_points(int num_points, double cube_size, int num_dimensions) {
        double[][] pts = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < num_points) {
            double[] p = ((double[])(new double[]{}));
            int j = 0;
            while (j < num_dimensions) {
                double v = cube_size * rand_float();
                p = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(p), java.util.stream.DoubleStream.of(v)).toArray()));
                j = j + 1;
            }
            pts = ((double[][])(appendObj(pts, p)));
            i = i + 1;
        }
        return pts;
    }

    static double[][] build_kdtree(double[][] points, int depth) {
        return points;
    }

    static double distance_sq(double[] a, double[] b) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < a.length) {
            double d = a[i_1] - b[i_1];
            sum = sum + d * d;
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static java.util.Map<String,Double> nearest_neighbour_search(double[][] points, double[] query) {
        if (points.length == 0) {
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("index", -1.0), java.util.Map.entry("dist", INF), java.util.Map.entry("visited", 0.0)));
        }
        int nearest_idx = 0;
        double nearest_dist = INF;
        int visited = 0;
        int i_2 = 0;
        while (i_2 < points.length) {
            double d_1 = distance_sq(((double[])(query)), ((double[])(points[i_2])));
            visited = visited + 1;
            if (d_1 < nearest_dist) {
                nearest_dist = d_1;
                nearest_idx = i_2;
            }
            i_2 = i_2 + 1;
        }
        return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("index", ((Number)(nearest_idx)).doubleValue()), java.util.Map.entry("dist", nearest_dist), java.util.Map.entry("visited", ((Number)(visited)).doubleValue())));
    }

    static void test_build_cases() {
        double[][] empty_pts = ((double[][])(new double[][]{}));
        double[][] tree0 = ((double[][])(build_kdtree(((double[][])(empty_pts)), 0)));
        if (tree0.length == 0) {
            System.out.println("case1 true");
        } else {
            System.out.println("case1 false");
        }
        double[][] pts1 = ((double[][])(hypercube_points(10, 10.0, 2)));
        double[][] tree1 = ((double[][])(build_kdtree(((double[][])(pts1)), 2)));
        if (tree1.length > 0 && tree1[0].length == 2) {
            System.out.println("case2 true");
        } else {
            System.out.println("case2 false");
        }
        double[][] pts2 = ((double[][])(hypercube_points(10, 10.0, 3)));
        double[][] tree2 = ((double[][])(build_kdtree(((double[][])(pts2)), -2)));
        if (tree2.length > 0 && tree2[0].length == 3) {
            System.out.println("case3 true");
        } else {
            System.out.println("case3 false");
        }
    }

    static void test_search() {
        double[][] pts_1 = ((double[][])(hypercube_points(10, 10.0, 2)));
        double[][] tree = ((double[][])(build_kdtree(((double[][])(pts_1)), 0)));
        double[] qp = ((double[])(hypercube_points(1, 10.0, 2)[0]));
        java.util.Map<String,Double> res = nearest_neighbour_search(((double[][])(tree)), ((double[])(qp)));
        if ((double)(((double)(res).getOrDefault("index", 0.0))) != (-1.0) && (double)(((double)(res).getOrDefault("dist", 0.0))) >= 0.0 && (double)(((double)(res).getOrDefault("visited", 0.0))) > 0.0) {
            System.out.println("search true");
        } else {
            System.out.println("search false");
        }
    }

    static void test_edge() {
        double[][] empty_pts_1 = ((double[][])(new double[][]{}));
        double[][] tree_1 = ((double[][])(build_kdtree(((double[][])(empty_pts_1)), 0)));
        double[] query = ((double[])(new double[]{0.0, 0.0}));
        java.util.Map<String,Double> res_1 = nearest_neighbour_search(((double[][])(tree_1)), ((double[])(query)));
        if ((double)(((double)(res_1).getOrDefault("index", 0.0))) == (-1.0) && (double)(((double)(res_1).getOrDefault("dist", 0.0))) > 100000000.0 && (double)(((double)(res_1).getOrDefault("visited", 0.0))) == 0.0) {
            System.out.println("edge true");
        } else {
            System.out.println("edge false");
        }
    }

    static void main() {
        seed = 1;
        test_build_cases();
        test_search();
        test_edge();
    }
    public static void main(String[] args) {
        INF = 1000000000.0;
        seed = 1;
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
