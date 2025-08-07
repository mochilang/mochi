public class Main {
    static int[] DOOMSDAY_LEAP;
    static int[] DOOMSDAY_NOT_LEAP;
    static java.util.Map<Integer,String> WEEK_DAY_NAMES;

    static String get_week_day(int year, int month, int day) {
        if (year < 100) {
            throw new RuntimeException(String.valueOf("year should be in YYYY format"));
        }
        if (month < 1 || month > 12) {
            throw new RuntimeException(String.valueOf("month should be between 1 to 12"));
        }
        if (day < 1 || day > 31) {
            throw new RuntimeException(String.valueOf("day should be between 1 to 31"));
        }
        int century = Math.floorDiv(year, 100);
        int century_anchor = Math.floorMod((5 * (Math.floorMod(century, 4)) + 2), 7);
        int centurian = Math.floorMod(year, 100);
        int centurian_m = Math.floorMod(centurian, 12);
        int dooms_day = Math.floorMod((((Number)((Math.floorDiv(centurian, 12)))).intValue() + centurian_m + ((Number)((Math.floorDiv(centurian_m, 4)))).intValue() + century_anchor), 7);
        int day_anchor = Math.floorMod(year, 4) != 0 || (centurian == 0 && Math.floorMod(year, 400) != 0) ? DOOMSDAY_NOT_LEAP[month - 1] : DOOMSDAY_LEAP[month - 1];
        int week_day = Math.floorMod((dooms_day + day - day_anchor), 7);
        if (week_day < 0) {
            week_day = week_day + 7;
        }
        return ((String)(WEEK_DAY_NAMES).get(week_day));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            DOOMSDAY_LEAP = ((int[])(new int[]{4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5}));
            DOOMSDAY_NOT_LEAP = ((int[])(new int[]{3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5}));
            WEEK_DAY_NAMES = ((java.util.Map<Integer,String>)(new java.util.LinkedHashMap<Integer, String>(java.util.Map.ofEntries(java.util.Map.entry(0, "Sunday"), java.util.Map.entry(1, "Monday"), java.util.Map.entry(2, "Tuesday"), java.util.Map.entry(3, "Wednesday"), java.util.Map.entry(4, "Thursday"), java.util.Map.entry(5, "Friday"), java.util.Map.entry(6, "Saturday")))));
            System.out.println(get_week_day(2020, 10, 24));
            System.out.println(get_week_day(2017, 10, 24));
            System.out.println(get_week_day(2019, 5, 3));
            System.out.println(get_week_day(1970, 9, 16));
            System.out.println(get_week_day(1870, 8, 13));
            System.out.println(get_week_day(2040, 3, 14));
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
