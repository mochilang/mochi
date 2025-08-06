public class Main {

    static int pow_int(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static int[] prime_factors(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Only positive integers have prime factors"));
        }
        int num = n;
        int[] pf = ((int[])(new int[]{}));
        while (Math.floorMod(num, 2) == 0) {
            pf = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pf), java.util.stream.IntStream.of(2)).toArray()));
            num = num / 2;
        }
        int i_1 = 3;
        while (i_1 * i_1 <= num) {
            while (Math.floorMod(num, i_1) == 0) {
                pf = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pf), java.util.stream.IntStream.of(i_1)).toArray()));
                num = num / i_1;
            }
            i_1 = i_1 + 2;
        }
        if (num > 2) {
            pf = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pf), java.util.stream.IntStream.of(num)).toArray()));
        }
        return pf;
    }

    static int number_of_divisors(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        int num_1 = n;
        int div = 1;
        int temp = 1;
        while (Math.floorMod(num_1, 2) == 0) {
            temp = temp + 1;
            num_1 = num_1 / 2;
        }
        div = div * temp;
        int i_2 = 3;
        while (i_2 * i_2 <= num_1) {
            temp = 1;
            while (Math.floorMod(num_1, i_2) == 0) {
                temp = temp + 1;
                num_1 = num_1 / i_2;
            }
            div = div * temp;
            i_2 = i_2 + 2;
        }
        if (num_1 > 1) {
            div = div * 2;
        }
        return div;
    }

    static int sum_of_divisors(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        int num_2 = n;
        int s = 1;
        int temp_1 = 1;
        while (Math.floorMod(num_2, 2) == 0) {
            temp_1 = temp_1 + 1;
            num_2 = num_2 / 2;
        }
        if (temp_1 > 1) {
            s = s * ((pow_int(2, temp_1) - 1) / (2 - 1));
        }
        int i_3 = 3;
        while (i_3 * i_3 <= num_2) {
            temp_1 = 1;
            while (Math.floorMod(num_2, i_3) == 0) {
                temp_1 = temp_1 + 1;
                num_2 = num_2 / i_3;
            }
            if (temp_1 > 1) {
                s = s * ((pow_int(i_3, temp_1) - 1) / (i_3 - 1));
            }
            i_3 = i_3 + 2;
        }
        return s;
    }

    static boolean contains(int[] arr, int x) {
        int idx = 0;
        while (idx < arr.length) {
            if (arr[idx] == x) {
                return true;
            }
            idx = idx + 1;
        }
        return false;
    }

    static int[] unique(int[] arr) {
        int[] result_1 = ((int[])(new int[]{}));
        int idx_1 = 0;
        while (idx_1 < arr.length) {
            int v = arr[idx_1];
            if (!(Boolean)contains(((int[])(result_1)), v)) {
                result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(v)).toArray()));
            }
            idx_1 = idx_1 + 1;
        }
        return result_1;
    }

    static int euler_phi(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Only positive numbers are accepted"));
        }
        int s_1 = n;
        int[] factors = ((int[])(unique(((int[])(prime_factors(n))))));
        int idx_2 = 0;
        while (idx_2 < factors.length) {
            int x = factors[idx_2];
            s_1 = (s_1 / x) * (x - 1);
            idx_2 = idx_2 + 1;
        }
        return s_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(prime_factors(100)));
            System.out.println(_p(number_of_divisors(100)));
            System.out.println(_p(sum_of_divisors(100)));
            System.out.println(_p(euler_phi(100)));
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
