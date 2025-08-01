public class Main {
    static double[] x1;
    static double[] x2;
    static double[] x3;

    static double[] sortFloat(double[] xs) {
        double[] arr = xs;
        int n = arr.length;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < n - 1) {
                if (arr[j] > arr[j + 1]) {
                    double t = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = t;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static int ceilf(double x) {
        int i_1 = ((Number)(x)).intValue();
        if (x > (((Number)(i_1)).doubleValue())) {
            return i_1 + 1;
        }
        return i_1;
    }

    static double[] fivenum(double[] a) {
        double[] arr_1 = sortFloat(a);
        int n_1 = arr_1.length;
        int half = (n_1 + 3) - (Math.floorMod((n_1 + 3), 2));
        double n4 = ((Number)((half / 2))).doubleValue() / 2.0;
        double nf = ((Number)(n_1)).doubleValue();
        double[] d = new double[]{1.0, n4, (nf + 1.0) / 2.0, nf + 1.0 - n4, nf};
        double[] result = new double[]{};
        int idx = 0;
        while (idx < d.length) {
            double de = d[idx];
            int fl = ((Number)(de - 1.0)).intValue();
            int cl = ceilf(de - 1.0);
            result = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(0.5 * (arr_1[fl] + arr_1[cl]))).toArray();
            idx = idx + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        x1 = new double[]{36.0, 40.0, 7.0, 39.0, 41.0, 15.0};
        x2 = new double[]{15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0};
        x3 = new double[]{0.14082834, 0.0974879, 1.73131507, 0.87636009, -1.95059594, 0.73438555, -0.03035726, 1.4667597, -0.74621349, -0.72588772, 0.6390516, 0.61501527, -0.9898378, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578};
        System.out.println(java.util.Arrays.toString(fivenum(x1)));
        System.out.println(java.util.Arrays.toString(fivenum(x2)));
        System.out.println(java.util.Arrays.toString(fivenum(x3)));
    }
}
