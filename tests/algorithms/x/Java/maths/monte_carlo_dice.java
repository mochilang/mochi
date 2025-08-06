public class Main {
    static int lcg_seed = 0;

    static int lcg_rand() {
        lcg_seed = ((int)(Math.floorMod(((long)((lcg_seed * 1103515245 + 12345))), 2147483648L)));
        return lcg_seed;
    }

    static int roll() {
        double rv = ((Number)(lcg_rand())).doubleValue();
        double r = rv * 6.0 / 2147483648.0;
        return 1 + (((Number)(r)).intValue());
    }

    static double round2(double x) {
        double y = x * 100.0 + 0.5;
        int z = ((Number)(y)).intValue();
        return (((Number)(z)).doubleValue()) / 100.0;
    }

    static double[] throw_dice(int num_throws, int num_dice) {
        int[] count_of_sum = ((int[])(new int[]{}));
        int max_sum = num_dice * 6 + 1;
        int i = 0;
        while (i < max_sum) {
            count_of_sum = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(count_of_sum), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int t = 0;
        while (t < num_throws) {
            int s = 0;
            int d = 0;
            while (d < num_dice) {
                s = s + roll();
                d = d + 1;
            }
count_of_sum[s] = count_of_sum[s] + 1;
            t = t + 1;
        }
        double[] probability = ((double[])(new double[]{}));
        i = num_dice;
        while (i < max_sum) {
            double p = (((double)(count_of_sum[i]))) * 100.0 / (((Number)(num_throws)).doubleValue());
            probability = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(probability), java.util.stream.DoubleStream.of(round2(p))).toArray()));
            i = i + 1;
        }
        return probability;
    }

    static void main() {
        lcg_seed = 1;
        double[] result = ((double[])(throw_dice(10000, 2)));
        System.out.println(_p(result));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            lcg_seed = 1;
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
