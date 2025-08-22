public class Main {
    static class EasterDate {
        long month;
        long day;
        EasterDate(long month, long day) {
            this.month = month;
            this.day = day;
        }
        EasterDate() {}
        @Override public String toString() {
            return String.format("{'month': %s, 'day': %s}", String.valueOf(month), String.valueOf(day));
        }
    }

    static long[] years = ((long[])(new long[]{1994, 2000, 2010, 2021, 2023, 2032, 2100}));
    static long i = 0L;

    static EasterDate gauss_easter(long year) {
        long metonic_cycle = Math.floorMod(year, 19);
        long julian_leap_year_1 = Math.floorMod(year, 4);
        long non_leap_year_1 = Math.floorMod(year, 7);
        long leap_day_inhibits_1 = Math.floorDiv(((long)(year)), ((long)(100)));
        long lunar_orbit_correction_1 = Math.floorDiv((13L + (long)(8L * (long)(leap_day_inhibits_1))), 25);
        double leap_day_reinstall_number_1 = (double)((double)((((Number)(leap_day_inhibits_1)).doubleValue())) / (double)(4.0));
        double secular_moon_shift_1 = (double)((double)(((double)((double)((double)(15.0) - (double)((((Number)(lunar_orbit_correction_1)).doubleValue()))) + (double)((((Number)(leap_day_inhibits_1)).doubleValue()))) - (double)(leap_day_reinstall_number_1))) % (double)(30.0));
        double century_starting_point_1 = (double)((double)(((double)((double)(4.0) + (double)((((Number)(leap_day_inhibits_1)).doubleValue()))) - (double)(leap_day_reinstall_number_1))) % (double)(7.0));
        double days_to_add_1 = (double)((double)(((double)((double)(19.0) * (double)((((Number)(metonic_cycle)).doubleValue()))) + (double)(secular_moon_shift_1))) % (double)(30.0));
        double days_from_phm_to_sunday_1 = (double)((double)(((double)((double)((double)((double)(2.0) * (double)((((Number)(julian_leap_year_1)).doubleValue()))) + (double)((double)(4.0) * (double)((((Number)(non_leap_year_1)).doubleValue())))) + (double)((double)(6.0) * (double)(days_to_add_1))) + (double)(century_starting_point_1))) % (double)(7.0));
        if ((double)(days_to_add_1) == (double)(29.0) && (double)(days_from_phm_to_sunday_1) == (double)(6.0)) {
            return new EasterDate(4, 19);
        }
        if ((double)(days_to_add_1) == (double)(28.0) && (double)(days_from_phm_to_sunday_1) == (double)(6.0)) {
            return new EasterDate(4, 18);
        }
        long offset_1 = (long)(((Number)(((double)(days_to_add_1) + (double)(days_from_phm_to_sunday_1)))).intValue());
        long total_1 = (long)(22L + (long)(offset_1));
        if ((long)(total_1) > 31L) {
            return new EasterDate(4, (long)(total_1) - 31L);
        }
        return new EasterDate(3, total_1);
    }

    static String format_date(long year, EasterDate d) {
        String month = String.valueOf((long)(d.month) < 10L ? "0" + _p(d.month) : _p(d.month));
        String day_1 = String.valueOf((long)(d.day) < 10L ? "0" + _p(d.day) : _p(d.day));
        return _p(year) + "-" + month + "-" + day_1;
    }
    public static void main(String[] args) {
        while ((long)(i) < (long)(years.length)) {
            long y = (long)(years[(int)((long)(i))]);
            EasterDate e = gauss_easter((long)(y));
            System.out.println("Easter in " + _p(y) + " is " + String.valueOf(format_date((long)(y), e)));
            i = (long)((long)(i) + 1L);
        }
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
