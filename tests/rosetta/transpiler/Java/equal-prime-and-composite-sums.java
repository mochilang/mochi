public class Main {

    static String commatize(int n) {
        String s = String.valueOf(n);
        int i = _runeLen(s) - 3;
        while (i >= 1) {
            s = s.substring(0, i) + "," + s.substring(i, _runeLen(s));
            i = i - 3;
        }
        return s;
    }

    static boolean[] primeSieve(int n) {
        boolean[] sieve = new boolean[]{};
        int i_1 = 0;
        while (i_1 <= n) {
            sieve = appendBool(sieve, false);
            i_1 = i_1 + 1;
        }
sieve[0] = true;
sieve[1] = true;
        int p = 2;
        while (p * p <= n) {
            if (!(Boolean)sieve[p]) {
                int m = p * p;
                while (m <= n) {
sieve[m] = true;
                    m = m + p;
                }
            }
            p = p + 1;
        }
        return sieve;
    }

    static int search(int[] xs, int target) {
        int low = 0;
        int high = xs.length;
        while (low < high) {
            int mid = (low + high) / 2;
            if (xs[mid] < target) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return low;
    }

    static void main() {
        int limit = 45000;
        boolean[] compMap = primeSieve(limit);
        int[] compSums = new int[]{};
        int[] primeSums = new int[]{};
        int csum = 0;
        int psum = 0;
        int i_2 = 2;
        while (i_2 <= limit) {
            if (compMap[i_2]) {
                csum = csum + i_2;
                compSums = java.util.stream.IntStream.concat(java.util.Arrays.stream(compSums), java.util.stream.IntStream.of(csum)).toArray();
            } else {
                psum = psum + i_2;
                primeSums = java.util.stream.IntStream.concat(java.util.Arrays.stream(primeSums), java.util.stream.IntStream.of(psum)).toArray();
            }
            i_2 = i_2 + 1;
        }
        System.out.println("Sum        | Prime Index | Composite Index");
        System.out.println("------------------------------------------");
        int idx = 0;
        while (idx < primeSums.length) {
            int s_1 = primeSums[idx];
            int j = search(compSums, s_1);
            if (j < compSums.length && compSums[j] == s_1) {
                Object sumStr = _padStart(commatize(s_1), 10, " ");
                Object piStr = _padStart(commatize(idx + 1), 11, " ");
                Object ciStr = _padStart(commatize(j + 1), 15, " ");
                System.out.println((String)(sumStr) + " | " + (String)(piStr) + " | " + (String)(ciStr));
            }
            idx = idx + 1;
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
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _padStart(String s, int width, String pad) {
        String out = s;
        while (out.length() < width) { out = pad + out; }
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
