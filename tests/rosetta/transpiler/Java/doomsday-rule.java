public class Main {
    static String[] days = new String[]{"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};
    static int[] firstDaysCommon = new int[]{3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5};
    static int[] firstDaysLeap = new int[]{4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5};

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i < _runeLen(str)) {
            n = n * 10 + (int)(((int)(digits).get(str.substring(i, i + 1))));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static int anchorDay(int y) {
        return Math.floorMod((2 + 5 * (Math.floorMod(y, 4)) + 4 * (Math.floorMod(y, 100)) + 6 * (Math.floorMod(y, 400))), 7);
    }

    static boolean isLeapYear(int y) {
        return Math.floorMod(y, 4) == 0 && (Math.floorMod(y, 100) != 0 || Math.floorMod(y, 400) == 0);
    }

    static void main() {
        String[] dates = new String[]{"1800-01-06", "1875-03-29", "1915-12-07", "1970-12-23", "2043-05-14", "2077-02-12", "2101-04-02"};
        System.out.println("Days of week given by Doomsday rule:");
        for (String date : dates) {
            int y = Integer.parseInt(date.substring(0, 4));
            int m = Integer.parseInt(date.substring(5, 7)) - 1;
            int d = Integer.parseInt(date.substring(8, 10));
            int a = anchorDay(y);
            int f = firstDaysCommon[m];
            if (isLeapYear(y)) {
                f = firstDaysLeap[m];
            }
            int w = d - f;
            if (w < 0) {
                w = 7 + w;
            }
            int dow = Math.floorMod((a + w), 7);
            System.out.println(date + " -> " + days[dow]);
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
