public class Main {

    static int min_partitions(String s) {
        int n = _runeLen(s);
        int[] cut = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            cut = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(cut), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        boolean[][] pal = ((boolean[][])(new boolean[][]{}));
        i = 0;
        while (i < n) {
            boolean[] row = ((boolean[])(new boolean[]{}));
            int j = 0;
            while (j < n) {
                row = ((boolean[])(appendBool(row, false)));
                j = j + 1;
            }
            pal = ((boolean[][])(appendObj(pal, row)));
            i = i + 1;
        }
        i = 0;
        while (i < n) {
            int mincut = i;
            int j_1 = 0;
            while (j_1 <= i) {
                if ((s.substring(i, i+1).equals(s.substring(j_1, j_1+1))) && (i - j_1 < 2 || ((Boolean)(pal[j_1 + 1][i - 1])))) {
pal[j_1][i] = true;
                    if (j_1 == 0) {
                        mincut = 0;
                    } else {
                        int candidate = cut[j_1 - 1] + 1;
                        if (candidate < mincut) {
                            mincut = candidate;
                        }
                    }
                }
                j_1 = j_1 + 1;
            }
cut[i] = mincut;
            i = i + 1;
        }
        return cut[n - 1];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(min_partitions("aab"));
            System.out.println(min_partitions("aaa"));
            System.out.println(min_partitions("ababbbabbababa"));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
