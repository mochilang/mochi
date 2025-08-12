public class Main {
    static class Item {
        double value;
        double weight;
        Item(double value, double weight) {
            this.value = value;
            this.weight = weight;
        }
        Item() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'weight': %s}", String.valueOf(value), String.valueOf(weight));
        }
    }

    static double[] vl;
    static double[] wt;
    static double result;

    static Item[] sort_by_ratio_desc(Item[] arr) {
        long i = 1L;
        while (i < arr.length) {
            Item key_1 = arr[(int)((long)(i))];
            long j_1 = i - 1;
            while (j_1 >= 0) {
                Item current_1 = arr[(int)((long)(j_1))];
                if (current_1.value / current_1.weight < key_1.value / key_1.weight) {
arr[(int)((long)(j_1 + 1))] = current_1;
                    j_1 = j_1 - 1;
                } else {
                    break;
                }
            }
arr[(int)((long)(j_1 + 1))] = key_1;
            i = i + 1;
        }
        return arr;
    }

    static double sum_first(double[] arr, long k) {
        double s = 0.0;
        long i_2 = 0L;
        while (i_2 < k && i_2 < arr.length) {
            s = s + arr[(int)((long)(i_2))];
            i_2 = i_2 + 1;
        }
        return s;
    }

    static double frac_knapsack(double[] vl, double[] wt, double w, long n) {
        Item[] items = ((Item[])(new Item[]{}));
        long i_4 = 0L;
        while (i_4 < vl.length && i_4 < wt.length) {
            items = ((Item[])(java.util.stream.Stream.concat(java.util.Arrays.stream(items), java.util.stream.Stream.of(new Item(vl[(int)((long)(i_4))], wt[(int)((long)(i_4))]))).toArray(Item[]::new)));
            i_4 = i_4 + 1;
        }
        items = ((Item[])(sort_by_ratio_desc(((Item[])(items)))));
        double[] values_1 = ((double[])(new double[]{}));
        double[] weights_1 = ((double[])(new double[]{}));
        i_4 = 0;
        while (i_4 < items.length) {
            Item itm_1 = items[(int)((long)(i_4))];
            values_1 = ((double[])(appendDouble(values_1, itm_1.value)));
            weights_1 = ((double[])(appendDouble(weights_1, itm_1.weight)));
            i_4 = i_4 + 1;
        }
        double[] acc_1 = ((double[])(new double[]{}));
        double total_1 = 0.0;
        i_4 = 0;
        while (i_4 < weights_1.length) {
            total_1 = total_1 + weights_1[(int)((long)(i_4))];
            acc_1 = ((double[])(appendDouble(acc_1, total_1)));
            i_4 = i_4 + 1;
        }
        long k_1 = 0L;
        while (k_1 < acc_1.length && w >= acc_1[(int)((long)(k_1))]) {
            k_1 = k_1 + 1;
        }
        if (k_1 == 0) {
            return 0.0;
        }
        if (k_1 >= values_1.length) {
            return sum_first(((double[])(values_1)), values_1.length);
        }
        if (k_1 != n) {
            return sum_first(((double[])(values_1)), k_1) + (w - acc_1[(int)((long)(k_1 - 1))]) * values_1[(int)((long)(k_1))] / weights_1[(int)((long)(k_1))];
        }
        return sum_first(((double[])(values_1)), k_1);
    }
    public static void main(String[] args) {
        vl = ((double[])(new double[]{60.0, 100.0, 120.0}));
        wt = ((double[])(new double[]{10.0, 20.0, 30.0}));
        result = frac_knapsack(((double[])(vl)), ((double[])(wt)), 50.0, 3L);
        System.out.println(_p(result));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
