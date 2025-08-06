public class Main {
    static int[] prices;

    static void enforce_args(int n, int[] prices) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n must be non-negative"));
        }
        if (n > prices.length) {
            throw new RuntimeException(String.valueOf("price list is shorter than n"));
        }
    }

    static int bottom_up_cut_rod(int n, int[] prices) {
        enforce_args(n, ((int[])(prices)));
        int[] max_rev = new int[0];
        int i = 0;
        while (i <= n) {
            if (i == 0) {
                max_rev = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(max_rev), java.util.stream.IntStream.of(0)).toArray()));
            } else {
                max_rev = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(max_rev), java.util.stream.IntStream.of(-(int)2147483648L)).toArray()));
            }
            i = i + 1;
        }
        int length = 1;
        while (length <= n) {
            int best = max_rev[length];
            int j = 1;
            while (j <= length) {
                int candidate = prices[j - 1] + max_rev[length - j];
                if (candidate > best) {
                    best = candidate;
                }
                j = j + 1;
            }
max_rev[length] = best;
            length = length + 1;
        }
        return max_rev[n];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            prices = ((int[])(new int[]{1, 5, 8, 9, 10, 17, 17, 20, 24, 30}));
            System.out.println(bottom_up_cut_rod(4, ((int[])(prices))));
            System.out.println(bottom_up_cut_rod(10, ((int[])(prices))));
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
