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
            points = ((double[][])(appendObj((double[][])points, point)));
            i = i + 1;
        }
        return points;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
            pts = ((double[][])(hypercube_points(3, 1.0, 2)));
            System.out.println(java.util.Arrays.deepToString(pts));
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
}
