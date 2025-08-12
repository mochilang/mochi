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

    static long[] years;
    static long i = 0;

    static EasterDate gauss_easter(long year) {
        long metonic_cycle = Math.floorMod(year, 19);
        long julian_leap_year_1 = Math.floorMod(year, 4);
        long non_leap_year_1 = Math.floorMod(year, 7);
        long leap_day_inhibits_1 = Math.floorDiv(year, 100);
        long lunar_orbit_correction_1 = Math.floorDiv(((long)(13) + (long)((long)(8) * (long)(leap_day_inhibits_1))), 25);
        double leap_day_reinstall_number_1 = (((Number)(leap_day_inhibits_1)).doubleValue()) / 4.0;
        double secular_moon_shift_1 = (15.0 - (((Number)(lunar_orbit_correction_1)).doubleValue()) + (((Number)(leap_day_inhibits_1)).doubleValue()) - leap_day_reinstall_number_1) % 30.0;
        double century_starting_point_1 = (4.0 + (((Number)(leap_day_inhibits_1)).doubleValue()) - leap_day_reinstall_number_1) % 7.0;
        double days_to_add_1 = (19.0 * (((Number)(metonic_cycle)).doubleValue()) + secular_moon_shift_1) % 30.0;
        double days_from_phm_to_sunday_1 = (2.0 * (((Number)(julian_leap_year_1)).doubleValue()) + 4.0 * (((Number)(non_leap_year_1)).doubleValue()) + 6.0 * days_to_add_1 + century_starting_point_1) % 7.0;
        if (days_to_add_1 == 29.0 && days_from_phm_to_sunday_1 == 6.0) {
            return new EasterDate(4, 19);
        }
        if (days_to_add_1 == 28.0 && days_from_phm_to_sunday_1 == 6.0) {
            return new EasterDate(4, 18);
        }
        long offset_1 = (long)(((Number)((days_to_add_1 + days_from_phm_to_sunday_1))).intValue());
        long total_1 = (long)((long)(22) + (long)(offset_1));
        if ((long)(total_1) > (long)(31)) {
            return new EasterDate(4, (long)(total_1) - (long)(31));
        }
        return new EasterDate(3, total_1);
    }

    static String format_date(long year, EasterDate d) {
        String month = String.valueOf((long)(d.month) < (long)(10) ? "0" + _p(d.month) : _p(d.month));
        String day_1 = String.valueOf((long)(d.day) < (long)(10) ? "0" + _p(d.day) : _p(d.day));
        return _p(year) + "-" + month + "-" + day_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            years = ((long[])(new long[]{1994, 2000, 2010, 2021, 2023, 2032, 2100}));
            i = (long)(0);
            while ((long)(i) < (long)(years.length)) {
                long y = (long)(years[(int)((long)(i))]);
                EasterDate e = gauss_easter((long)(y));
                System.out.println("Easter in " + _p(y) + " is " + String.valueOf(format_date((long)(y), e)));
                i = (long)((long)(i) + (long)(1));
            }
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
