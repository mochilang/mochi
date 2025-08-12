public class Main {
    static long p;
    static long a;
    static long b_2;
    static long left;
    static long right_fast;
    static long right_naive;

    static long binary_exponentiation(long a, long n, long mod) {
        if (n == 0) {
            return 1;
        }
        if (Math.floorMod(n, 2) == 1) {
            return Math.floorMod((binary_exponentiation(a, n - 1, mod) * a), mod);
        }
        long b_1 = binary_exponentiation(a, Math.floorDiv(n, 2), mod);
        return Math.floorMod((b_1 * b_1), mod);
    }

    static long naive_exponent_mod(long a, long n, long mod) {
        long result = 1;
        long i_1 = 0;
        while (i_1 < n) {
            result = Math.floorMod((result * a), mod);
            i_1 = i_1 + 1;
        }
        return result;
    }

    static void print_bool(boolean b) {
        if (((Boolean)(b))) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            p = 701;
            a = 1000000000;
            b_2 = 10;
            left = Math.floorMod((Math.floorDiv(a, b_2)), p);
            right_fast = Math.floorMod((a * binary_exponentiation(b_2, p - 2, p)), p);
            print_bool(left == right_fast);
            right_naive = Math.floorMod((a * naive_exponent_mod(b_2, p - 2, p)), p);
            print_bool(left == right_naive);
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
