public class Main {

    static long[] bubble_sort(long[] xs) {
        long[] arr = ((long[])(xs));
        long n = arr.length;
        long i = 0;
        while (i < n) {
            long j = 0;
            while (j < n - i - 1) {
                if (arr[j] > arr[j + 1]) {
                    long tmp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static long[] factors(long num) {
        long[] values = ((long[])(new long[]{1}));
        long i_1 = 2;
        while (i_1 * i_1 <= num) {
            if (Math.floorMod(num, i_1) == 0) {
                values = ((long[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(values), java.util.stream.IntStream.of(i_1)).toArray()));
                Object d = Math.floorDiv(num, i_1);
                if (((Number)(d)).intValue() != i_1) {
                    values = ((long[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(values), java.util.stream.IntStream.of(((Number)(d)).intValue())).toArray()));
                }
            }
            i_1 = i_1 + 1;
        }
        return bubble_sort(((long[])(values)));
    }

    static long sum_list(long[] xs) {
        long total = 0;
        long i_2 = 0;
        while (i_2 < xs.length) {
            total = total + xs[i_2];
            i_2 = i_2 + 1;
        }
        return total;
    }

    static boolean abundant(long n) {
        return sum_list(((long[])(factors(n)))) > n;
    }

    static boolean semi_perfect(long number) {
        if (number <= 0) {
            return true;
        }
        long[] values_1 = ((long[])(factors(number)));
        boolean[] possible = ((boolean[])(new boolean[]{}));
        long j_1 = 0;
        while (j_1 <= number) {
            possible = ((boolean[])(appendBool(possible, j_1 == 0)));
            j_1 = j_1 + 1;
        }
        long idx = 0;
        while (idx < values_1.length) {
            long v = values_1[idx];
            long s = number;
            while (s >= v) {
                if (((Boolean)(possible[s - v]))) {
possible[s] = true;
                }
                s = s - 1;
            }
            idx = idx + 1;
        }
        return possible[number];
    }

    static boolean weird(long number) {
        return ((Boolean)(abundant(number))) && semi_perfect(number) == false;
    }

    static void run_tests() {
        if (factors(12) != new long[]{1, 2, 3, 4, 6}) {
            throw new RuntimeException(String.valueOf("factors 12 failed"));
        }
        if (factors(1) != new long[]{1}) {
            throw new RuntimeException(String.valueOf("factors 1 failed"));
        }
        if (factors(100) != new long[]{1, 2, 4, 5, 10, 20, 25, 50}) {
            throw new RuntimeException(String.valueOf("factors 100 failed"));
        }
        if (abundant(0) != true) {
            throw new RuntimeException(String.valueOf("abundant 0 failed"));
        }
        if (abundant(1) != false) {
            throw new RuntimeException(String.valueOf("abundant 1 failed"));
        }
        if (abundant(12) != true) {
            throw new RuntimeException(String.valueOf("abundant 12 failed"));
        }
        if (abundant(13) != false) {
            throw new RuntimeException(String.valueOf("abundant 13 failed"));
        }
        if (abundant(20) != true) {
            throw new RuntimeException(String.valueOf("abundant 20 failed"));
        }
        if (semi_perfect(0) != true) {
            throw new RuntimeException(String.valueOf("semi_perfect 0 failed"));
        }
        if (semi_perfect(1) != true) {
            throw new RuntimeException(String.valueOf("semi_perfect 1 failed"));
        }
        if (semi_perfect(12) != true) {
            throw new RuntimeException(String.valueOf("semi_perfect 12 failed"));
        }
        if (semi_perfect(13) != false) {
            throw new RuntimeException(String.valueOf("semi_perfect 13 failed"));
        }
        if (weird(0) != false) {
            throw new RuntimeException(String.valueOf("weird 0 failed"));
        }
        if (weird(70) != true) {
            throw new RuntimeException(String.valueOf("weird 70 failed"));
        }
        if (weird(77) != false) {
            throw new RuntimeException(String.valueOf("weird 77 failed"));
        }
    }

    static void main() {
        run_tests();
        long[] nums = ((long[])(new long[]{69, 70, 71}));
        long i_3 = 0;
        while (i_3 < nums.length) {
            long n_1 = nums[i_3];
            if (((Boolean)(weird(n_1)))) {
                System.out.println(_p(n_1) + " is weird.");
            } else {
                System.out.println(_p(n_1) + " is not weird.");
            }
            i_3 = i_3 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
    static long _nowSeed;
    static long _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Long.parseLong(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (_nowSeed * 1664525L + 1013904223) % 2147483647L;
            return _nowSeed;
        }
        return System.nanoTime() / 1000;
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
