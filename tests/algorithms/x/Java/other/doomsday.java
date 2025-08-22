public class Main {
    static long[] DOOMSDAY_LEAP = ((long[])(new long[]{4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5}));
    static long[] DOOMSDAY_NOT_LEAP = ((long[])(new long[]{3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5}));
    static java.util.Map<Long,String> WEEK_DAY_NAMES;

    static String get_week_day(long year, long month, long day) {
        if ((long)(year) < 100L) {
            throw new RuntimeException(String.valueOf("year should be in YYYY format"));
        }
        if ((long)(month) < 1L || (long)(month) > 12L) {
            throw new RuntimeException(String.valueOf("month should be between 1 to 12"));
        }
        if ((long)(day) < 1L || (long)(day) > 31L) {
            throw new RuntimeException(String.valueOf("day should be between 1 to 31"));
        }
        long century_1 = Math.floorDiv(((long)(year)), ((long)(100)));
        long century_anchor_1 = Math.floorMod(((long)(5L * (long)((Math.floorMod(century_1, 4)))) + 2L), 7);
        long centurian_1 = Math.floorMod(year, 100);
        long centurian_m_1 = Math.floorMod(centurian_1, 12);
        long dooms_day_1 = Math.floorMod(((long)((long)(((Number)((Math.floorDiv(centurian_1, 12)))).intValue() + (long)(centurian_m_1)) + ((Number)((Math.floorDiv(centurian_m_1, 4)))).intValue()) + (long)(century_anchor_1)), 7);
        long day_anchor_1 = (long)(Math.floorMod(year, 4) != 0L || ((long)(centurian_1) == 0L && Math.floorMod(year, 400) != 0L) ? DOOMSDAY_NOT_LEAP[(int)((long)((long)(month) - 1L))] : DOOMSDAY_LEAP[(int)((long)((long)(month) - 1L))]);
        long week_day_1 = Math.floorMod(((long)((long)(dooms_day_1) + (long)(day)) - (long)(day_anchor_1)), 7);
        if ((long)(week_day_1) < 0L) {
            week_day_1 = (long)((long)(week_day_1) + 7L);
        }
        return ((String)(WEEK_DAY_NAMES).get(week_day_1));
    }
    public static void main(String[] args) {
        WEEK_DAY_NAMES = ((java.util.Map<Long,String>)(new java.util.LinkedHashMap<Long, String>(java.util.Map.ofEntries(java.util.Map.entry(0L, "Sunday"), java.util.Map.entry(1L, "Monday"), java.util.Map.entry(2L, "Tuesday"), java.util.Map.entry(3L, "Wednesday"), java.util.Map.entry(4L, "Thursday"), java.util.Map.entry(5L, "Friday"), java.util.Map.entry(6L, "Saturday")))));
        System.out.println(get_week_day(2020L, 10L, 24L));
        System.out.println(get_week_day(2017L, 10L, 24L));
        System.out.println(get_week_day(2019L, 5L, 3L));
        System.out.println(get_week_day(1970L, 9L, 16L));
        System.out.println(get_week_day(1870L, 8L, 13L));
        System.out.println(get_week_day(2040L, 3L, 14L));
    }
}
