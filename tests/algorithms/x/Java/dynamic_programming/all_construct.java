public class Main {

    static String[][] allConstruct(String target, String[] wordBank) {
        int tableSize = _runeLen(target) + 1;
        String[][][] table = ((String[][][])(new String[][][]{}));
        int idx = 0;
        while (idx < tableSize) {
            String[][] empty = ((String[][])(new String[][]{}));
            table = ((String[][][])(appendObj(table, empty)));
            idx = idx + 1;
        }
        String[] base = ((String[])(new String[]{}));
table[0] = ((String[][])(new String[][]{base}));
        int i = 0;
        while (i < tableSize) {
            if (table[i].length != 0) {
                int w = 0;
                while (w < wordBank.length) {
                    String word = wordBank[w];
                    int wordLen = _runeLen(word);
                    if ((target.substring(i, i + wordLen).equals(word))) {
                        int k = 0;
                        while (k < table[i].length) {
                            String[] way = ((String[])(table[i][k]));
                            String[] combination = ((String[])(new String[]{}));
                            int m = 0;
                            while (m < way.length) {
                                combination = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(combination), java.util.stream.Stream.of(way[m])).toArray(String[]::new)));
                                m = m + 1;
                            }
                            combination = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(combination), java.util.stream.Stream.of(word)).toArray(String[]::new)));
                            int nextIndex = i + wordLen;
table[nextIndex] = ((String[][])(appendObj(table[nextIndex], combination)));
                            k = k + 1;
                        }
                    }
                    w = w + 1;
                }
            }
            i = i + 1;
        }
        return table[_runeLen(target)];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(allConstruct("jwajalapa", ((String[])(new String[]{"jwa", "j", "w", "a", "la", "lapa"})))));
            System.out.println(_p(allConstruct("rajamati", ((String[])(new String[]{"s", "raj", "amat", "raja", "ma", "i", "t"})))));
            System.out.println(_p(allConstruct("hexagonosaurus", ((String[])(new String[]{"h", "ex", "hex", "ag", "ago", "ru", "auru", "rus", "go", "no", "o", "s"})))));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
