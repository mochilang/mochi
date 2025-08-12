public class Main {
    static long lcg_seed = 0;

    static long lcg_rand() {
        lcg_seed = ((long)(Math.floorMod(((long)((lcg_seed * 1103515245 + 12345))), 2147483648L)));
        return lcg_seed;
    }

    static long roll() {
        double rv = ((Number)(lcg_rand())).doubleValue();
        double r_1 = rv * 6.0 / 2147483648.0;
        return 1 + (((Number)(r_1)).intValue());
    }

    static double round2(double x) {
        double y = x * 100.0 + 0.5;
        long z_1 = ((Number)(y)).intValue();
        return (((Number)(z_1)).doubleValue()) / 100.0;
    }

    static double[] throw_dice(long num_throws, long num_dice) {
        long[] count_of_sum = ((long[])(new long[]{}));
        long max_sum_1 = num_dice * 6 + 1;
        long i_1 = 0;
        while (i_1 < max_sum_1) {
            count_of_sum = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(count_of_sum), java.util.stream.LongStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        long t_1 = 0;
        while (t_1 < num_throws) {
            long s_1 = 0;
            long d_1 = 0;
            while (d_1 < num_dice) {
                s_1 = s_1 + roll();
                d_1 = d_1 + 1;
            }
count_of_sum[(int)(s_1)] = count_of_sum[(int)(s_1)] + 1;
            t_1 = t_1 + 1;
        }
        double[] probability_1 = ((double[])(new double[]{}));
        i_1 = num_dice;
        while (i_1 < max_sum_1) {
            double p_1 = (((double)(count_of_sum[(int)(i_1)]))) * 100.0 / (((Number)(num_throws)).doubleValue());
            probability_1 = ((double[])(appendDouble(probability_1, round2(p_1))));
            i_1 = i_1 + 1;
        }
        return probability_1;
    }

    static void main() {
        lcg_seed = 1;
        double[] result_1 = ((double[])(throw_dice(10000, 2)));
        System.out.println(_p(result_1));
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
