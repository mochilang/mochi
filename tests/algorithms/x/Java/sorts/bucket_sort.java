public class Main {

    static double int_to_float(int x) {
        return x * 1.0;
    }

    static int floor_int(double x) {
        int i = 0;
        while (int_to_float(i + 1) <= x) {
            i = i + 1;
        }
        return i;
    }

    static double[] set_at_float(double[] xs, int idx, double value) {
        int i_1 = 0;
        double[] res = ((double[])(new double[]{}));
        while (i_1 < xs.length) {
            if (i_1 == idx) {
                res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(value)).toArray()));
            } else {
                res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(xs[i_1])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static double[][] set_at_list_float(double[][] xs, int idx, double[] value) {
        int i_2 = 0;
        double[][] res_1 = ((double[][])(new double[][]{}));
        while (i_2 < xs.length) {
            if (i_2 == idx) {
                res_1 = ((double[][])(appendObj(res_1, value)));
            } else {
                res_1 = ((double[][])(appendObj(res_1, xs[i_2])));
            }
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static double[] sort_float(double[] xs) {
        double[] res_2 = ((double[])(xs));
        int i_3 = 1;
        while (i_3 < res_2.length) {
            double key = res_2[i_3];
            int j = i_3 - 1;
            while (j >= 0 && res_2[j] > key) {
                res_2 = ((double[])(set_at_float(((double[])(res_2)), j + 1, res_2[j])));
                j = j - 1;
            }
            res_2 = ((double[])(set_at_float(((double[])(res_2)), j + 1, key)));
            i_3 = i_3 + 1;
        }
        return res_2;
    }

    static double[] bucket_sort_with_count(double[] xs, int bucket_count) {
        if (xs.length == 0 || bucket_count <= 0) {
            return new double[]{};
        }
        double min_value = xs[0];
        double max_value = xs[0];
        int i_4 = 1;
        while (i_4 < xs.length) {
            if (xs[i_4] < min_value) {
                min_value = xs[i_4];
            }
            if (xs[i_4] > max_value) {
                max_value = xs[i_4];
            }
            i_4 = i_4 + 1;
        }
        if (max_value == min_value) {
            return xs;
        }
        double bucket_size = (max_value - min_value) / int_to_float(bucket_count);
        double[][] buckets = ((double[][])(new double[][]{}));
        i_4 = 0;
        while (i_4 < bucket_count) {
            buckets = ((double[][])(appendObj(buckets, new double[]{})));
            i_4 = i_4 + 1;
        }
        i_4 = 0;
        while (i_4 < xs.length) {
            double val = xs[i_4];
            int idx = floor_int((val - min_value) / bucket_size);
            if (idx < 0) {
                idx = 0;
            }
            if (idx >= bucket_count) {
                idx = bucket_count - 1;
            }
            double[] bucket = ((double[])(buckets[idx]));
            bucket = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(bucket), java.util.stream.DoubleStream.of(val)).toArray()));
            buckets = ((double[][])(set_at_list_float(((double[][])(buckets)), idx, ((double[])(bucket)))));
            i_4 = i_4 + 1;
        }
        double[] result = ((double[])(new double[]{}));
        i_4 = 0;
        while (i_4 < buckets.length) {
            double[] sorted_bucket = ((double[])(sort_float(((double[])(buckets[i_4])))));
            int j_1 = 0;
            while (j_1 < sorted_bucket.length) {
                result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(sorted_bucket[j_1])).toArray()));
                j_1 = j_1 + 1;
            }
            i_4 = i_4 + 1;
        }
        return result;
    }

    static double[] bucket_sort(double[] xs) {
        return bucket_sort_with_count(((double[])(xs)), 10);
    }
    public static void main(String[] args) {
        System.out.println(_p(bucket_sort(((double[])(new double[]{-1.0, 2.0, -5.0, 0.0})))));
        System.out.println(_p(bucket_sort(((double[])(new double[]{9.0, 8.0, 7.0, 6.0, -12.0})))));
        System.out.println(_p(bucket_sort(((double[])(new double[]{0.4, 1.2, 0.1, 0.2, -0.9})))));
        System.out.println(_p(bucket_sort(((double[])(new double[]{})))));
        System.out.println(_p(bucket_sort(((double[])(new double[]{-10000000000.0, 10000000000.0})))));
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
