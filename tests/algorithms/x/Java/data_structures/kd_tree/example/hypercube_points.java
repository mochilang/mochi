public class Main {
    static int seed = 0;
    static double[][] pts;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (((Number)(rand())).doubleValue()) / 2147483648.0;
    }

    static double[][] hypercube_points(int num_points, double hypercube_size, int num_dimensions) {
        double[][] points = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < num_points) {
            double[] point = ((double[])(new double[]{}));
            int j = 0;
            while (j < num_dimensions) {
                double value = hypercube_size * random();
                point = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(point), java.util.stream.DoubleStream.of(value)).toArray()));
                j = j + 1;
            }
            points = ((double[][])(appendObj(points, point)));
            i = i + 1;
        }
        return points;
    }
    public static void main(String[] args) {
        seed = 1;
        pts = ((double[][])(hypercube_points(3, 1.0, 2)));
        System.out.println(java.util.Arrays.deepToString(pts));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
