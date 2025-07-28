public class Main {

    static boolean damm(String s) {
        int[][] tbl = new int[][]{new int[]{0, 3, 1, 7, 5, 9, 8, 6, 4, 2}, new int[]{7, 0, 9, 2, 1, 5, 4, 8, 6, 3}, new int[]{4, 2, 0, 6, 8, 7, 1, 3, 5, 9}, new int[]{1, 7, 5, 0, 9, 8, 3, 4, 2, 6}, new int[]{6, 1, 2, 3, 0, 4, 5, 9, 7, 8}, new int[]{3, 6, 7, 4, 2, 0, 9, 5, 8, 1}, new int[]{5, 8, 6, 9, 7, 2, 0, 1, 3, 4}, new int[]{8, 9, 4, 5, 3, 6, 2, 0, 1, 7}, new int[]{9, 4, 3, 8, 6, 1, 7, 2, 0, 5}, new int[]{2, 5, 8, 1, 4, 3, 6, 7, 9, 0}};
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        int interim = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            int digit = (int)(((int)(digits).get(s.substring(i, i + 1))));
            int[] row = tbl[interim];
            interim = row[digit];
            i = i + 1;
        }
        return interim == 0;
    }

    static String padLeft(String s, int width) {
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static void main() {
        for (String s : new String[]{"5724", "5727", "112946", "112949"}) {
            System.out.println(String.valueOf(padLeft(s, 6)) + "  " + String.valueOf(damm(s)));
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
}
