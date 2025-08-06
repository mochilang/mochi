public class Main {
    static class NevilleResult {
        double value;
        double[][] table;
        NevilleResult(double value, double[][] table) {
            this.value = value;
            this.table = table;
        }
        NevilleResult() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'table': %s}", String.valueOf(value), String.valueOf(table));
        }
    }


    static NevilleResult neville_interpolate(double[] x_points, double[] y_points, double x0) {
        int n = x_points.length;
        double[][] q = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < n) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < n) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                j = j + 1;
            }
            q = ((double[][])(appendObj(q, row)));
            i = i + 1;
        }
        i = 0;
        while (i < n) {
q[i][1] = y_points[i];
            i = i + 1;
        }
        int col = 2;
        while (col < n) {
            int row_idx = col;
            while (row_idx < n) {
q[row_idx][col] = ((x0 - x_points[row_idx - col + 1]) * q[row_idx][col - 1] - (x0 - x_points[row_idx]) * q[row_idx - 1][col - 1]) / (x_points[row_idx] - x_points[row_idx - col + 1]);
                row_idx = row_idx + 1;
            }
            col = col + 1;
        }
        return new NevilleResult(q[n - 1][n - 1], q);
    }

    static void test_neville() {
        double[] xs = ((double[])(new double[]{1.0, 2.0, 3.0, 4.0, 6.0}));
        double[] ys = ((double[])(new double[]{6.0, 7.0, 8.0, 9.0, 11.0}));
        NevilleResult r1 = neville_interpolate(((double[])(xs)), ((double[])(ys)), 5.0);
        if (r1.value != 10.0) {
            throw new RuntimeException(String.valueOf("neville_interpolate at 5 failed"));
        }
        NevilleResult r2 = neville_interpolate(((double[])(xs)), ((double[])(ys)), 99.0);
        if (r2.value != 104.0) {
            throw new RuntimeException(String.valueOf("neville_interpolate at 99 failed"));
        }
    }

    static void main() {
        test_neville();
        double[] xs_1 = ((double[])(new double[]{1.0, 2.0, 3.0, 4.0, 6.0}));
        double[] ys_1 = ((double[])(new double[]{6.0, 7.0, 8.0, 9.0, 11.0}));
        NevilleResult r = neville_interpolate(((double[])(xs_1)), ((double[])(ys_1)), 5.0);
        System.out.println(r.value);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
}
