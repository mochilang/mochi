public class Main {

    static int pfacSum(int i) {
        int sum = 0;
        int p = 1;
        while (p <= i / 2) {
            if (i % p == 0) {
                sum = sum + p;
            }
            p = p + 1;
        }
        return sum;
    }

    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = String.valueOf(" " + s);
        }
        return s;
    }

    static void main() {
        int[] sums = new int[]{};
        int i = 0;
        while (i < 20000) {
            sums = java.util.stream.IntStream.concat(java.util.Arrays.stream(sums), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        i = 1;
        while (i < 20000) {
sums[i] = pfacSum(i);
            i = i + 1;
        }
        System.out.println("The amicable pairs below 20,000 are:");
        int n = 2;
        while (n < 19999) {
            int m = sums[n];
            if (m > n && m < 20000 && n == sums[m]) {
                System.out.println(String.valueOf("  " + String.valueOf(pad(n, 5))) + " and " + pad(m, 5));
            }
            n = n + 1;
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
