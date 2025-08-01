public class Main {
    static double[] x1;
    static double[] x2;
    static double[] x3;

    static double qsel(double[] a, int k) {
        double[] arr = a;
        while (arr.length > 1) {
            int px = Math.floorMod(_now(), arr.length);
            double pv = arr[px];
            int last = arr.length - 1;
            double tmp = arr[px];
arr[px] = arr[last];
arr[last] = tmp;
            px = 0;
            int i = 0;
            while (i < last) {
                double v = arr[i];
                if (v < pv) {
                    double t = arr[px];
arr[px] = arr[i];
arr[i] = t;
                    px = px + 1;
                }
                i = i + 1;
            }
arr[px] = pv;
            if (px == k) {
                return pv;
            }
            if (k < px) {
                arr = java.util.Arrays.copyOfRange(arr, 0, px);
            } else {
                arr = java.util.Arrays.copyOfRange(arr, (px + 1), arr.length);
                k = k - (px + 1);
            }
        }
        return arr[0];
    }

    static double[] fivenum(double[] a) {
        int last_1 = a.length - 1;
        int m = last_1 / 2;
        double[] n5 = new double[]{};
        n5 = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(n5), java.util.stream.DoubleStream.of(qsel(java.util.Arrays.copyOfRange(a, 0, m), 0))).toArray();
        n5 = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(n5), java.util.stream.DoubleStream.of(qsel(java.util.Arrays.copyOfRange(a, 0, m), a.length / 4))).toArray();
        n5 = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(n5), java.util.stream.DoubleStream.of(qsel(a, m))).toArray();
        double[] arr2 = java.util.Arrays.copyOfRange(a, m, a.length);
        int q3 = last_1 - m - a.length / 4;
        n5 = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(n5), java.util.stream.DoubleStream.of(qsel(arr2, q3))).toArray();
        arr2 = java.util.Arrays.copyOfRange(arr2, q3, arr2.length);
        n5 = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(n5), java.util.stream.DoubleStream.of(qsel(arr2, arr2.length - 1))).toArray();
        return n5;
    }
    public static void main(String[] args) {
        x1 = new double[]{36.0, 40.0, 7.0, 39.0, 41.0, 15.0};
        x2 = new double[]{15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0};
        x3 = new double[]{0.14082834, 0.0974879, 1.73131507, 0.87636009, -1.95059594, 0.73438555, -0.03035726, 1.4667597, -0.74621349, -0.72588772, 0.6390516, 0.61501527, -0.9898378, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578};
        System.out.println(String.valueOf(fivenum(x1)));
        System.out.println(String.valueOf(fivenum(x2)));
        System.out.println(String.valueOf(fivenum(x3)));
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
}
