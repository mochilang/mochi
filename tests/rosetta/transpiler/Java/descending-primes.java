public class Main {
    static int[] digits = new int[]{9, 8, 7, 6, 5, 4, 3, 2, 1};
    static int[] primes = gen(0, 0, false);
    static int i = 0;
    static String line = "";

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        int d = 3;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
        }
        return true;
    }

    static int[] gen(int idx, int cur, boolean used) {
        if (idx == digits.length) {
            if (used && isPrime(cur)) {
                return new int[]{cur};
            }
            return new int[]{};
        }
        int[] with = gen(idx + 1, cur * 10 + digits[idx], true);
        int[] without = gen(idx + 1, cur, used);
        return java.util.stream.IntStream.concat(java.util.Arrays.stream(with), java.util.Arrays.stream(without)).toArray();
    }

    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("There are " + String.valueOf(primes.length) + " descending primes, namely:");
            while (i < primes.length) {
                line = line + String.valueOf(pad(primes[i], 8)) + " ";
                if (Math.floorMod((i + 1), 10) == 0) {
                    System.out.println(line.substring(0, line.length() - 1));
                    line = "";
                }
                i = i + 1;
            }
            if (line.length() > 0) {
                System.out.println(line.substring(0, line.length() - 1));
            }
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
