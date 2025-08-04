public class Main {

    static boolean[] sieve(int limit) {
        boolean[] primes = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i < limit) {
            primes = ((boolean[])(appendBool(primes, true)));
            i = i + 1;
        }
primes[0] = false;
primes[1] = false;
        int p = 2;
        while (p * p < limit) {
            if (((Boolean)(primes[p]))) {
                int k = p * p;
                while (k < limit) {
primes[k] = false;
                    k = k + p;
                }
            }
            p = p + 1;
        }
        return primes;
    }

    static int goldbachCount(boolean[] primes, int n) {
        int c = 0;
        int i_1 = 1;
        while (i_1 <= n / 2) {
            if (((Boolean)(primes[i_1])) && ((Boolean)(primes[n - i_1]))) {
                c = c + 1;
            }
            i_1 = i_1 + 1;
        }
        return c;
    }

    static String pad(int n) {
        if (n < 10) {
            return "  " + _p(n);
        }
        if (n < 100) {
            return " " + _p(n);
        }
        return _p(n);
    }

    static void main() {
        boolean[] primes_1 = ((boolean[])(sieve(1000)));
        System.out.println("The first 100 Goldbach numbers:");
        String line = "";
        int n = 2;
        int count = 0;
        while (count < 100) {
            int v = goldbachCount(((boolean[])(primes_1)), 2 * n);
            line = line + String.valueOf(pad(v)) + " ";
            count = count + 1;
            n = n + 1;
            if (Math.floorMod(count, 10) == 0) {
                System.out.println(_substr(line, 0, _runeLen(line) - 1));
                line = "";
            }
        }
        int val = goldbachCount(((boolean[])(primes_1)), 1000);
        System.out.println("\nThe 1,000th Goldbach number = " + _p(val));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
