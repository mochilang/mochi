public class Main {
    static double[][] m = new double[][]{new double[]{2.0, -1.0, 5.0, 1.0}, new double[]{3.0, 2.0, 2.0, -6.0}, new double[]{1.0, 3.0, 3.0, -1.0}, new double[]{5.0, -2.0, -3.0, 3.0}};
    static double[] v = new double[]{-3.0, -32.0, -47.0, 49.0};
    static double d = det(m);
    static double[] x = new double[]{};
    static int i = 0;
    static String s = "[";
    static int j = 0;

    static double det(double[][] m) {
        int n = m.length;
        if (n == 1) {
            return m[0][0];
        }
        double total = 0.0;
        double sign = 1.0;
        int c = 0;
        while (c < n) {
            double[][] sub = new double[][]{};
            int r = 1;
            while (r < n) {
                double[] row = new double[]{};
                int cc = 0;
                while (cc < n) {
                    if (cc != c) {
                        row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(m[r][cc])).toArray();
                    }
                    cc = cc + 1;
                }
                sub = appendObj(sub, row);
                r = r + 1;
            }
            total = total + sign * m[0][c] * det(sub);
            sign = sign * (-1.0);
            c = c + 1;
        }
        return total;
    }

    static double[][] replaceCol(double[][] m, int col, double[] v) {
        double[][] res = new double[][]{};
        int r = 0;
        while (r < m.length) {
            double[] row = new double[]{};
            int c = 0;
            while (c < m[r].length) {
                if (c == col) {
                    row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(v[r])).toArray();
                } else {
                    row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(m[r][c])).toArray();
                }
                c = c + 1;
            }
            res = appendObj(res, row);
            r = r + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        while (i < v.length) {
            double[][] mc = replaceCol(m, i, v);
            x = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(x), java.util.stream.DoubleStream.of(det(mc) / d)).toArray();
            i = i + 1;
        }
        while (j < x.length) {
            s = s + String.valueOf(x[j]);
            if (j < x.length - 1) {
                s = s + " ";
            }
            j = j + 1;
        }
        s = s + "]";
        System.out.println(s);
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
