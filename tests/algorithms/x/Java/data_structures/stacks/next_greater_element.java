public class Main {
    static double[] arr;
    static double[] expected;

    static double[] next_greatest_element_slow(double[] xs) {
        double[] res = ((double[])(new double[]{}));
        int i = 0;
        while (i < xs.length) {
            double next = -1.0;
            int j = i + 1;
            while (j < xs.length) {
                if (xs[i] < xs[j]) {
                    next = xs[j];
                    break;
                }
                j = j + 1;
            }
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(next)).toArray()));
            i = i + 1;
        }
        return res;
    }

    static double[] next_greatest_element_fast(double[] xs) {
        double[] res_1 = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            double next_1 = -1.0;
            int j_1 = i_1 + 1;
            while (j_1 < xs.length) {
                double inner = xs[j_1];
                if (xs[i_1] < inner) {
                    next_1 = inner;
                    break;
                }
                j_1 = j_1 + 1;
            }
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(next_1)).toArray()));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static double[] set_at_float(double[] xs, int idx, double value) {
        int i_2 = 0;
        double[] res_2 = ((double[])(new double[]{}));
        while (i_2 < xs.length) {
            if (i_2 == idx) {
                res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(value)).toArray()));
            } else {
                res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(xs[i_2])).toArray()));
            }
            i_2 = i_2 + 1;
        }
        return res_2;
    }

    static double[] next_greatest_element(double[] xs) {
        double[] res_3 = ((double[])(new double[]{}));
        int k = 0;
        while (k < xs.length) {
            res_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_3), java.util.stream.DoubleStream.of(-1.0)).toArray()));
            k = k + 1;
        }
        int[] stack = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < xs.length) {
            while (stack.length > 0 && xs[i_3] > xs[stack[stack.length - 1]]) {
                int idx = stack[stack.length - 1];
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                res_3 = ((double[])(set_at_float(((double[])(res_3)), idx, xs[i_3])));
            }
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(i_3)).toArray()));
            i_3 = i_3 + 1;
        }
        return res_3;
    }
    public static void main(String[] args) {
        arr = ((double[])(new double[]{-10.0, -5.0, 0.0, 5.0, 5.1, 11.0, 13.0, 21.0, 3.0, 4.0, -21.0, -10.0, -5.0, -1.0, 0.0}));
        expected = ((double[])(new double[]{-5.0, 0.0, 5.0, 5.1, 11.0, 13.0, 21.0, -1.0, 4.0, -1.0, -10.0, -5.0, -1.0, 0.0, -1.0}));
        System.out.println(_p(next_greatest_element_slow(((double[])(arr)))));
        System.out.println(_p(next_greatest_element_fast(((double[])(arr)))));
        System.out.println(_p(next_greatest_element(((double[])(arr)))));
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
