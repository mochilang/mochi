public class Main {

    static int fuscVal(int n) {
        int a = 1;
        int b = 0;
        int x = n;
        while (x > 0) {
            if (Math.floorMod(x, 2) == 0) {
                x = x / 2;
                a = a + b;
            } else {
                x = (x - 1) / 2;
                b = a + b;
            }
        }
        if (n == 0) {
            return 0;
        }
        return b;
    }

    static int[] firstFusc(int n) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(fuscVal(i))).toArray()));
            i = i + 1;
        }
        return arr;
    }

    static String commatize(int n) {
        String s = _p(n);
        boolean neg = false;
        if (n < 0) {
            neg = true;
            s = _substr(s, 1, _runeLen(s));
        }
        int i_1 = _runeLen(s) - 3;
        while (i_1 >= 1) {
            s = _substr(s, 0, i_1) + "," + _substr(s, i_1, _runeLen(s));
            i_1 = i_1 - 3;
        }
        if (neg) {
            return "-" + s;
        }
        return s;
    }

    static String padLeft(String s, int w) {
        String out = s;
        while (_runeLen(out) < w) {
            out = " " + out;
        }
        return out;
    }

    static void main() {
        System.out.println("The first 61 fusc numbers are:");
        System.out.println(_p(firstFusc(61)));
        System.out.println("\nThe fusc numbers whose length > any previous fusc number length are:");
        int[] idxs = ((int[])(new int[]{0, 37, 1173, 35499, 699051, 19573419}));
        int i_2 = 0;
        while (i_2 < idxs.length) {
            int idx = idxs[i_2];
            int val = fuscVal(idx);
            String numStr = String.valueOf(padLeft(String.valueOf(commatize(val)), 7));
            String idxStr = String.valueOf(padLeft(String.valueOf(commatize(idx)), 10));
            System.out.println(numStr + " (index " + idxStr + ")");
            i_2 = i_2 + 1;
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
