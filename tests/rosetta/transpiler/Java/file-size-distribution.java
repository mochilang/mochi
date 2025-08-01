public class Main {

    static int log10floor(int n) {
        int p = 0;
        int v = n;
        while (v >= 10) {
            v = ((Number)((v / 10))).intValue();
            p = p + 1;
        }
        return p;
    }

    static String commatize(int n) {
        String s = String.valueOf(n);
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            if (i > 0 && Math.floorMod((_runeLen(s) - i), 3) == 0) {
                res = res + ",";
            }
            res = res + _substr(s, i, i + 1);
            i = i + 1;
        }
        return res;
    }

    static void showDistribution(int[] sizes) {
        int[] bins = new int[]{};
        int i_1 = 0;
        while (i_1 < 12) {
            bins = java.util.stream.IntStream.concat(java.util.Arrays.stream(bins), java.util.stream.IntStream.of(0)).toArray();
            i_1 = i_1 + 1;
        }
        int total = 0;
        for (int sz : sizes) {
            total = total + sz;
            int idx = 0;
            if (sz > 0) {
                idx = log10floor(sz) + 1;
            }
bins[idx] = bins[idx] + 1;
        }
        System.out.println("File size distribution:\n");
        i_1 = 0;
        while (i_1 < bins.length) {
            String prefix = "  ";
            if (i_1 > 0) {
                prefix = "+ ";
            }
            System.out.println(prefix + "Files less than 10 ^ " + String.valueOf(i_1) + " bytes : " + String.valueOf(bins[i_1]));
            i_1 = i_1 + 1;
        }
        System.out.println("                                  -----");
        System.out.println("= Total number of files         : " + String.valueOf(sizes.length));
        System.out.println("  Total size of files           : " + String.valueOf(commatize(total)) + " bytes");
    }

    static void main() {
        int[] sizes = new int[]{0, 1, 9, 10, 99, 100, 1234, 50000, 730000, 8200000};
        showDistribution(sizes);
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
}
