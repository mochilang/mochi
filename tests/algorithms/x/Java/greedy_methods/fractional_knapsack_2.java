public class Main {
    static class KnapsackResult {
        double max_value;
        double[] fractions;
        KnapsackResult(double max_value, double[] fractions) {
            this.max_value = max_value;
            this.fractions = fractions;
        }
        KnapsackResult() {}
        @Override public String toString() {
            return String.format("{'max_value': %s, 'fractions': %s}", String.valueOf(max_value), String.valueOf(fractions));
        }
    }

    static double[] v;
    static double[] w;

    static int[] sort_by_ratio(int[] index, double[] ratio) {
        int i = 1;
        while (i < index.length) {
            int key = index[i];
            double key_ratio = ratio[key];
            int j = i - 1;
            while (j >= 0 && ratio[index[j]] < key_ratio) {
index[j + 1] = index[j];
                j = j - 1;
            }
index[j + 1] = key;
            i = i + 1;
        }
        return index;
    }

    static KnapsackResult fractional_knapsack(double[] value, double[] weight, double capacity) {
        int n = value.length;
        int[] index = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            index = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(index), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        double[] ratio = ((double[])(new double[]{}));
        i_1 = 0;
        while (i_1 < n) {
            ratio = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ratio), java.util.stream.DoubleStream.of(value[i_1] / weight[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        index = ((int[])(sort_by_ratio(((int[])(index)), ((double[])(ratio)))));
        double[] fractions = ((double[])(new double[]{}));
        i_1 = 0;
        while (i_1 < n) {
            fractions = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(fractions), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_1 = i_1 + 1;
        }
        double max_value = 0.0;
        int idx = 0;
        while (idx < index.length) {
            int item = index[idx];
            if (weight[item] <= capacity) {
fractions[item] = 1.0;
                max_value = max_value + value[item];
                capacity = capacity - weight[item];
            } else {
fractions[item] = capacity / weight[item];
                max_value = max_value + value[item] * capacity / weight[item];
                break;
            }
            idx = idx + 1;
        }
        return new KnapsackResult(max_value, fractions);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            v = ((double[])(new double[]{1.0, 3.0, 5.0, 7.0, 9.0}));
            w = ((double[])(new double[]{0.9, 0.7, 0.5, 0.3, 0.1}));
            System.out.println(fractional_knapsack(((double[])(v)), ((double[])(w)), 5.0));
            System.out.println(fractional_knapsack(((double[])(new double[]{1.0, 3.0, 5.0, 7.0})), ((double[])(new double[]{0.9, 0.7, 0.5, 0.3})), 30.0));
            System.out.println(fractional_knapsack(((double[])(new double[]{})), ((double[])(new double[]{})), 30.0));
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
}
