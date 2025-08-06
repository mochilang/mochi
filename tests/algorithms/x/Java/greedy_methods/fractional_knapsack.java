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
        int i = 1;
        while (i < arr.length) {
            Item key = arr[i];
            int j = i - 1;
            while (j >= 0) {
                Item current = arr[j];
                if (current.value / current.weight < key.value / key.weight) {
arr[j + 1] = current;
                    j = j - 1;
                } else {
                    break;
                }
            }
arr[j + 1] = key;
            i = i + 1;
        }
        return arr;
    }

    static double sum_first(double[] arr, int k) {
        double s = 0.0;
        int i_1 = 0;
        while (i_1 < k && i_1 < arr.length) {
            s = s + arr[i_1];
            i_1 = i_1 + 1;
        }
        return s;
    }

    static double frac_knapsack(double[] vl, double[] wt, double w, int n) {
        Item[] items = ((Item[])(new Item[]{}));
        int i_2 = 0;
        while (i_2 < vl.length && i_2 < wt.length) {
            items = ((Item[])(java.util.stream.Stream.concat(java.util.Arrays.stream(items), java.util.stream.Stream.of(new Item(vl[i_2], wt[i_2]))).toArray(Item[]::new)));
            i_2 = i_2 + 1;
        }
        items = ((Item[])(sort_by_ratio_desc(((Item[])(items)))));
        double[] values = ((double[])(new double[]{}));
        double[] weights = ((double[])(new double[]{}));
        i_2 = 0;
        while (i_2 < items.length) {
            Item itm = items[i_2];
            values = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(values), java.util.stream.DoubleStream.of(itm.value)).toArray()));
            weights = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(weights), java.util.stream.DoubleStream.of(itm.weight)).toArray()));
            i_2 = i_2 + 1;
        }
        double[] acc = ((double[])(new double[]{}));
        double total = 0.0;
        i_2 = 0;
        while (i_2 < weights.length) {
            total = total + weights[i_2];
            acc = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(acc), java.util.stream.DoubleStream.of(total)).toArray()));
            i_2 = i_2 + 1;
        }
        int k = 0;
        while (k < acc.length && w >= acc[k]) {
            k = k + 1;
        }
        if (k == 0) {
            return 0.0;
        }
        if (k >= values.length) {
            return sum_first(((double[])(values)), values.length);
        }
        if (k != n) {
            return sum_first(((double[])(values)), k) + (w - acc[k - 1]) * values[k] / weights[k];
        }
        return sum_first(((double[])(values)), k);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            vl = ((double[])(new double[]{60.0, 100.0, 120.0}));
            wt = ((double[])(new double[]{10.0, 20.0, 30.0}));
            result = frac_knapsack(((double[])(vl)), ((double[])(wt)), 50.0, 3);
            System.out.println(_p(result));
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
