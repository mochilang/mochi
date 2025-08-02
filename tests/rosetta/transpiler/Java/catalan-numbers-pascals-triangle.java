public class Main {
    static int n;
    static int[] t;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            n = 15;
            t = new int[]{};
            for (int _v = 0; _v < (n + 2); _v++) {
                t = java.util.stream.IntStream.concat(java.util.Arrays.stream(t), java.util.stream.IntStream.of(0)).toArray();
            }
t[1] = 1;
            for (int i = 1; i < (n + 1); i++) {
                int j = i;
                while (j > 1) {
t[j] = t[j] + t[j - 1];
                    j = j - 1;
                }
t[((Number)((i + 1))).intValue()] = t[i];
                j = i + 1;
                while (j > 1) {
t[j] = t[j] + t[j - 1];
                    j = j - 1;
                }
                int cat = t[i + 1] - t[i];
                if (i < 10) {
                    System.out.println(" " + _p(i) + " : " + _p(cat));
                } else {
                    System.out.println(_p(i) + " : " + _p(cat));
                }
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
