public class Main {

    static String floyd(int n) {
        String result = "";
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < n - i - 1) {
                result = result + " ";
                j = j + 1;
            }
            int k = 0;
            while (k < i + 1) {
                result = result + "* ";
                k = k + 1;
            }
            result = result + "\n";
            i = i + 1;
        }
        return result;
    }

    static String reverse_floyd(int n) {
        String result_1 = "";
        int i_1 = n;
        while (i_1 > 0) {
            int j_1 = i_1;
            while (j_1 > 0) {
                result_1 = result_1 + "* ";
                j_1 = j_1 - 1;
            }
            result_1 = result_1 + "\n";
            int k_1 = n - i_1 + 1;
            while (k_1 > 0) {
                result_1 = result_1 + " ";
                k_1 = k_1 - 1;
            }
            i_1 = i_1 - 1;
        }
        return result_1;
    }

    static String pretty_print(int n) {
        if (n <= 0) {
            return "       ...       ....        nothing printing :(";
        }
        String upper_half = String.valueOf(floyd(n));
        String lower_half = String.valueOf(reverse_floyd(n));
        return upper_half + lower_half;
    }

    static void main() {
        System.out.println(pretty_print(3));
        System.out.println(pretty_print(0));
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
