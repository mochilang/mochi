public class Main {

    static int min_int(int a, int b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static int int_sqrt(int n) {
        int r = 0;
        while ((r + 1) * (r + 1) <= n) {
            r = r + 1;
        }
        return r;
    }

    static int[] sieve(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Number must instead be a positive integer"));
        }
        int[] in_prime = ((int[])(new int[]{}));
        int start = 2;
        int end = int_sqrt(n);
        int[] temp = ((int[])(new int[]{}));
        int i = 0;
        while (i < end + 1) {
            temp = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(temp), java.util.stream.IntStream.of(1)).toArray()));
            i = i + 1;
        }
        int[] prime = ((int[])(new int[]{}));
        while (start <= end) {
            if (temp[start] == 1) {
                in_prime = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(in_prime), java.util.stream.IntStream.of(start)).toArray()));
                int j = start * start;
                while (j <= end) {
temp[j] = 0;
                    j = j + start;
                }
            }
            start = start + 1;
        }
        i = 0;
        while (i < in_prime.length) {
            prime = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prime), java.util.stream.IntStream.of(in_prime[i])).toArray()));
            i = i + 1;
        }
        int low = end + 1;
        int high = min_int(2 * end, n);
        while (low <= n) {
            int[] tempSeg = ((int[])(new int[]{}));
            int size = high - low + 1;
            int k = 0;
            while (k < size) {
                tempSeg = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(tempSeg), java.util.stream.IntStream.of(1)).toArray()));
                k = k + 1;
            }
            int idx = 0;
            while (idx < in_prime.length) {
                int each = in_prime[idx];
                int t = (low / each) * each;
                if (t < low) {
                    t = t + each;
                }
                int j2 = t;
                while (j2 <= high) {
tempSeg[j2 - low] = 0;
                    j2 = j2 + each;
                }
                idx = idx + 1;
            }
            int j3 = 0;
            while (j3 < tempSeg.length) {
                if (tempSeg[j3] == 1) {
                    prime = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prime), java.util.stream.IntStream.of(j3 + low)).toArray()));
                }
                j3 = j3 + 1;
            }
            low = high + 1;
            high = min_int(high + end, n);
        }
        return prime;
    }

    static boolean lists_equal(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        int m = 0;
        while (m < a.length) {
            if (a[m] != b[m]) {
                return false;
            }
            m = m + 1;
        }
        return true;
    }

    static void test_sieve() {
        int[] e1 = ((int[])(sieve(8)));
        if (!(Boolean)lists_equal(((int[])(e1)), ((int[])(new int[]{2, 3, 5, 7})))) {
            throw new RuntimeException(String.valueOf("sieve(8) failed"));
        }
        int[] e2 = ((int[])(sieve(27)));
        if (!(Boolean)lists_equal(((int[])(e2)), ((int[])(new int[]{2, 3, 5, 7, 11, 13, 17, 19, 23})))) {
            throw new RuntimeException(String.valueOf("sieve(27) failed"));
        }
    }

    static void main() {
        test_sieve();
        System.out.println(_p(sieve(30)));
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
