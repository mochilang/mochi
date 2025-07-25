public class Main {

    static boolean kPrime(int n, int k) {
        int nf = 0;
        int i = 2;
        while (i <= n) {
            while (n % i == 0) {
                if (nf == k) {
                    return false;
                }
                nf = nf + 1;
                n = n / i;
            }
            i = i + 1;
        }
        return nf == k;
    }

    static int[] gen(int k, int count) {
        int[] r = new int[]{};
        int n = 2;
        while (r.length < count) {
            if (kPrime(n, k)) {
                r = java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(n)).toArray();
            }
            n = n + 1;
        }
        return r;
    }

    static void main() {
        int k = 1;
        while (k <= 5) {
            System.out.println(String.valueOf(String.valueOf(k) + " ") + String.valueOf(gen(k, 10)));
            k = k + 1;
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
