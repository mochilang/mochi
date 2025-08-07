public class Main {
    static class EasterDate {
        int month;
        int day;
        EasterDate(int month, int day) {
            this.month = month;
            this.day = day;
        }
        EasterDate() {}
        @Override public String toString() {
            return String.format("{'month': %s, 'day': %s}", String.valueOf(month), String.valueOf(day));
        }
    }

    static int[] years;
    static int i = 0;

    static EasterDate gauss_easter(int year) {
        int metonic_cycle = Math.floorMod(year, 19);
        int julian_leap_year = Math.floorMod(year, 4);
        int non_leap_year = Math.floorMod(year, 7);
        int leap_day_inhibits = Math.floorDiv(year, 100);
        int lunar_orbit_correction = Math.floorDiv((13 + 8 * leap_day_inhibits), 25);
        double leap_day_reinstall_number = (((Number)(leap_day_inhibits)).doubleValue()) / 4.0;
        double secular_moon_shift = (15.0 - (((Number)(lunar_orbit_correction)).doubleValue()) + (((Number)(leap_day_inhibits)).doubleValue()) - leap_day_reinstall_number) % 30.0;
        double century_starting_point = (4.0 + (((Number)(leap_day_inhibits)).doubleValue()) - leap_day_reinstall_number) % 7.0;
        double days_to_add = (19.0 * (((Number)(metonic_cycle)).doubleValue()) + secular_moon_shift) % 30.0;
        double days_from_phm_to_sunday = (2.0 * (((Number)(julian_leap_year)).doubleValue()) + 4.0 * (((Number)(non_leap_year)).doubleValue()) + 6.0 * days_to_add + century_starting_point) % 7.0;
        if (days_to_add == 29.0 && days_from_phm_to_sunday == 6.0) {
            return new EasterDate(4, 19);
        }
        if (days_to_add == 28.0 && days_from_phm_to_sunday == 6.0) {
            return new EasterDate(4, 18);
        }
        int offset = ((Number)((days_to_add + days_from_phm_to_sunday))).intValue();
        int total = 22 + offset;
        if (total > 31) {
            return new EasterDate(4, total - 31);
        }
        return new EasterDate(3, total);
    }

    static String format_date(int year, EasterDate d) {
        String month = String.valueOf(d.month < 10 ? "0" + _p(d.month) : _p(d.month));
        String day = String.valueOf(d.day < 10 ? "0" + _p(d.day) : _p(d.day));
        return _p(year) + "-" + month + "-" + day;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            years = ((int[])(new int[]{1994, 2000, 2010, 2021, 2023, 2032, 2100}));
            i = 0;
            while (i < years.length) {
                int y = years[i];
                EasterDate e = gauss_easter(y);
                System.out.println("Easter in " + _p(y) + " is " + String.valueOf(format_date(y, e)));
                i = i + 1;
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
        return String.valueOf(v);
    }
}
