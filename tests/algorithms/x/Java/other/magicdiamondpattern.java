public class Main {

    static String floyd(long n) {
        String result = "";
        long i_1 = 0L;
        while ((long)(i_1) < n) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)((long)(n - (long)(i_1)) - (long)(1))) {
                result = result + " ";
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            long k_1 = 0L;
            while ((long)(k_1) < (long)((long)(i_1) + (long)(1))) {
                result = result + "* ";
                k_1 = (long)((long)(k_1) + (long)(1));
            }
            result = result + "\n";
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return result;
    }

    static String reverse_floyd(long n) {
        String result_1 = "";
        long i_3 = n;
        while ((long)(i_3) > (long)(0)) {
            long j_3 = (long)(i_3);
            while ((long)(j_3) > (long)(0)) {
                result_1 = result_1 + "* ";
                j_3 = (long)((long)(j_3) - (long)(1));
            }
            result_1 = result_1 + "\n";
            long k_3 = (long)((long)(n - (long)(i_3)) + (long)(1));
            while ((long)(k_3) > (long)(0)) {
                result_1 = result_1 + " ";
                k_3 = (long)((long)(k_3) - (long)(1));
            }
            i_3 = (long)((long)(i_3) - (long)(1));
        }
        return result_1;
    }

    static String pretty_print(long n) {
        if (n <= (long)(0)) {
            return "       ...       ....        nothing printing :(";
        }
        String upper_half_1 = String.valueOf(floyd(n));
        String lower_half_1 = String.valueOf(reverse_floyd(n));
        return upper_half_1 + lower_half_1;
    }

    static void main() {
        System.out.println(pretty_print(3L));
        System.out.println(pretty_print(0L));
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
