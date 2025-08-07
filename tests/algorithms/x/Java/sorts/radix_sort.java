public class Main {
    static int RADIX;

    static int[][] make_buckets() {
        int[][] buckets = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < RADIX) {
            buckets = ((int[][])(appendObj(buckets, new int[]{})));
            i = i + 1;
        }
        return buckets;
    }

    static int max_value(int[] xs) {
        int max_val = xs[0];
        int i_1 = 1;
        while (i_1 < xs.length) {
            if (xs[i_1] > max_val) {
                max_val = xs[i_1];
            }
            i_1 = i_1 + 1;
        }
        return max_val;
    }

    static int[] radix_sort(int[] list_of_ints) {
        int placement = 1;
        int max_digit = max_value(((int[])(list_of_ints)));
        while (placement <= max_digit) {
            int[][] buckets_1 = ((int[][])(make_buckets()));
            int i_2 = 0;
            while (i_2 < list_of_ints.length) {
                int value = list_of_ints[i_2];
                int tmp = Math.floorMod((Math.floorDiv(value, placement)), RADIX);
buckets_1[tmp] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(buckets_1[tmp]), java.util.stream.IntStream.of(value)).toArray()));
                i_2 = i_2 + 1;
            }
            int a = 0;
            int b = 0;
            while (b < RADIX) {
                int[] bucket = ((int[])(buckets_1[b]));
                int j = 0;
                while (j < bucket.length) {
list_of_ints[a] = bucket[j];
                    a = a + 1;
                    j = j + 1;
                }
                b = b + 1;
            }
            placement = placement * RADIX;
        }
        return list_of_ints;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            RADIX = 10;
            System.out.println(_p(radix_sort(((int[])(new int[]{0, 5, 3, 2, 2})))));
            System.out.println(_p(radix_sort(((int[])(new int[]{1, 100, 10, 1000})))));
            System.out.println(_p(radix_sort(((int[])(new int[]{15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0})))));
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
