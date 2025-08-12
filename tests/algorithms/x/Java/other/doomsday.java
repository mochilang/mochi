public class Main {
    static long[] DOOMSDAY_LEAP;
    static long[] DOOMSDAY_NOT_LEAP;
    static java.util.Map<Long,String> WEEK_DAY_NAMES;

    static String get_week_day(long year, long month, long day) {
        if (year < (long)(100)) {
            throw new RuntimeException(String.valueOf("year should be in YYYY format"));
        }
        if (month < (long)(1) || month > (long)(12)) {
            throw new RuntimeException(String.valueOf("month should be between 1 to 12"));
        }
        if (day < (long)(1) || day > (long)(31)) {
            throw new RuntimeException(String.valueOf("day should be between 1 to 31"));
        }
        long century_1 = Math.floorDiv(year, 100);
        long century_anchor_1 = Math.floorMod(((long)((long)(5) * (long)((Math.floorMod(century_1, 4)))) + (long)(2)), 7);
        long centurian_1 = Math.floorMod(year, 100);
        long centurian_m_1 = Math.floorMod(centurian_1, 12);
        long dooms_day_1 = Math.floorMod(((long)((long)(((Number)((Math.floorDiv(centurian_1, 12)))).intValue() + (long)(centurian_m_1)) + ((Number)((Math.floorDiv(centurian_m_1, 4)))).intValue()) + (long)(century_anchor_1)), 7);
        long day_anchor_1 = Math.floorMod(year, 4) != (long)(0) || ((long)(centurian_1) == (long)(0) && Math.floorMod(year, 400) != (long)(0)) ? DOOMSDAY_NOT_LEAP[(int)((long)(month - (long)(1)))] : DOOMSDAY_LEAP[(int)((long)(month - (long)(1)))];
        long week_day_1 = Math.floorMod(((long)((long)(dooms_day_1) + day) - day_anchor_1), 7);
        if ((long)(week_day_1) < (long)(0)) {
            week_day_1 = (long)((long)(week_day_1) + (long)(7));
        }
        return ((String)(WEEK_DAY_NAMES).get(week_day_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            DOOMSDAY_LEAP = ((long[])(new long[]{4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5}));
            DOOMSDAY_NOT_LEAP = ((long[])(new long[]{3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5}));
            WEEK_DAY_NAMES = ((java.util.Map<Long,String>)(new java.util.LinkedHashMap<Long, String>(java.util.Map.ofEntries(java.util.Map.entry(0L, "Sunday"), java.util.Map.entry(1L, "Monday"), java.util.Map.entry(2L, "Tuesday"), java.util.Map.entry(3L, "Wednesday"), java.util.Map.entry(4L, "Thursday"), java.util.Map.entry(5L, "Friday"), java.util.Map.entry(6L, "Saturday")))));
            System.out.println(get_week_day(2020L, 10L, 24L));
            System.out.println(get_week_day(2017L, 10L, 24L));
            System.out.println(get_week_day(2019L, 5L, 3L));
            System.out.println(get_week_day(1970L, 9L, 16L));
            System.out.println(get_week_day(1870L, 8L, 13L));
            System.out.println(get_week_day(2040L, 3L, 14L));
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
