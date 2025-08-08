public class Main {
    static int[][] img;
    static double[][] pts1;
    static double[][] pts2;
    static int[][] rotated;

    static double[][] mat_inverse3(double[][] m) {
        double a = m[0][0];
        double b = m[0][1];
        double c = m[0][2];
        double d = m[1][0];
        double e = m[1][1];
        double f = m[1][2];
        double g = m[2][0];
        double h = m[2][1];
        double i = m[2][2];
        double det = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g);
        if (det == 0.0) {
            throw new RuntimeException(String.valueOf("singular matrix"));
        }
        double adj00 = e * i - f * h;
        double adj01 = c * h - b * i;
        double adj02 = b * f - c * e;
        double adj10 = f * g - d * i;
        double adj11 = a * i - c * g;
        double adj12 = c * d - a * f;
        double adj20 = d * h - e * g;
        double adj21 = b * g - a * h;
        double adj22 = a * e - b * d;
        double[][] inv = ((double[][])(new double[][]{}));
        inv = ((double[][])(appendObj((double[][])inv, new double[]{adj00 / det, adj01 / det, adj02 / det})));
        inv = ((double[][])(appendObj((double[][])inv, new double[]{adj10 / det, adj11 / det, adj12 / det})));
        inv = ((double[][])(appendObj((double[][])inv, new double[]{adj20 / det, adj21 / det, adj22 / det})));
        return inv;
    }

    static double[] mat_vec_mul(double[][] m, double[] v) {
        double[] res = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < 3) {
            double val = m[i_1][0] * v[0] + m[i_1][1] * v[1] + m[i_1][2] * v[2];
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(val)).toArray()));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int[][] create_matrix(int rows, int cols, int value) {
        int[][] result = ((int[][])(new int[][]{}));
        int r = 0;
        while (r < rows) {
            int[] row = ((int[])(new int[]{}));
            int c_1 = 0;
            while (c_1 < cols) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(value)).toArray()));
                c_1 = c_1 + 1;
            }
            result = ((int[][])(appendObj((int[][])result, row)));
            r = r + 1;
        }
        return result;
    }

    static int round_to_int(double x) {
        if (x >= 0.0) {
            return ((Number)(x + 0.5)).intValue();
        }
        return ((Number)(x - 0.5)).intValue();
    }

    static int[][] get_rotation(int[][] img, double[][] pt1, double[][] pt2, int rows, int cols) {
        Object[][] src = ((Object[][])(new Object[][]{new Object[]{pt1[0][0], pt1[0][1], 1.0}, new Object[]{pt1[1][0], pt1[1][1], 1.0}, new Object[]{pt1[2][0], pt1[2][1], 1.0}}));
        double[][] inv_1 = ((double[][])(mat_inverse3(((double[][])(src)))));
        double[] vecx = ((double[])(new double[]{pt2[0][0], pt2[1][0], pt2[2][0]}));
        double[] vecy = ((double[])(new double[]{pt2[0][1], pt2[1][1], pt2[2][1]}));
        double[] avec = ((double[])(mat_vec_mul(((double[][])(inv_1)), ((double[])(vecx)))));
        double[] bvec = ((double[])(mat_vec_mul(((double[][])(inv_1)), ((double[])(vecy)))));
        double a0 = avec[0];
        double a1 = avec[1];
        double a2 = avec[2];
        double b0 = bvec[0];
        double b1 = bvec[1];
        double b2 = bvec[2];
        int[][] out = ((int[][])(create_matrix(rows, cols, 0)));
        int y = 0;
        while (y < rows) {
            int x = 0;
            while (x < cols) {
                double xf = a0 * (1.0 * x) + a1 * (1.0 * y) + a2;
                double yf = b0 * (1.0 * x) + b1 * (1.0 * y) + b2;
                int sx = round_to_int(xf);
                int sy = round_to_int(yf);
                if (sx >= 0 && sx < cols && sy >= 0 && sy < rows) {
out[sy][sx] = img[y][x];
                }
                x = x + 1;
            }
            y = y + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        img = ((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{4, 5, 6}, new int[]{7, 8, 9}}));
        pts1 = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{2.0, 0.0}, new double[]{0.0, 2.0}}));
        pts2 = ((double[][])(new double[][]{new double[]{0.0, 2.0}, new double[]{0.0, 0.0}, new double[]{2.0, 2.0}}));
        rotated = ((int[][])(get_rotation(((int[][])(img)), ((double[][])(pts1)), ((double[][])(pts2)), 3, 3)));
        System.out.println(_p(rotated));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
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
}
