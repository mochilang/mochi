public class Main {
    static java.math.BigInteger[] DOOMSDAY_LEAP = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(5)}));
    static java.math.BigInteger[] DOOMSDAY_NOT_LEAP = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(5)}));
    static java.util.Map<java.math.BigInteger,String> WEEK_DAY_NAMES;

    static String get_week_day(java.math.BigInteger year, java.math.BigInteger month, java.math.BigInteger day) {
        if (year.compareTo(java.math.BigInteger.valueOf(100)) < 0) {
            throw new RuntimeException(String.valueOf("year should be in YYYY format"));
        }
        if (month.compareTo(java.math.BigInteger.valueOf(1)) < 0 || month.compareTo(java.math.BigInteger.valueOf(12)) > 0) {
            throw new RuntimeException(String.valueOf("month should be between 1 to 12"));
        }
        if (day.compareTo(java.math.BigInteger.valueOf(1)) < 0 || day.compareTo(java.math.BigInteger.valueOf(31)) > 0) {
            throw new RuntimeException(String.valueOf("day should be between 1 to 31"));
        }
        java.math.BigInteger century_1 = new java.math.BigInteger(String.valueOf(year.divide(java.math.BigInteger.valueOf(100))));
        java.math.BigInteger century_anchor_1 = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(5).multiply((century_1.remainder(java.math.BigInteger.valueOf(4)))).add(java.math.BigInteger.valueOf(2))).remainder(java.math.BigInteger.valueOf(7))));
        java.math.BigInteger centurian_1 = new java.math.BigInteger(String.valueOf(year.remainder(java.math.BigInteger.valueOf(100))));
        java.math.BigInteger centurian_m_1 = new java.math.BigInteger(String.valueOf(centurian_1.remainder(java.math.BigInteger.valueOf(12))));
        java.math.BigInteger dooms_day_1 = new java.math.BigInteger(String.valueOf(((centurian_1.divide(java.math.BigInteger.valueOf(12))).add(centurian_m_1).add((centurian_m_1.divide(java.math.BigInteger.valueOf(4)))).add(century_anchor_1)).remainder(java.math.BigInteger.valueOf(7))));
        java.math.BigInteger day_anchor_1 = new java.math.BigInteger(String.valueOf(year.remainder(java.math.BigInteger.valueOf(4)).compareTo(java.math.BigInteger.valueOf(0)) != 0 || (centurian_1.compareTo(java.math.BigInteger.valueOf(0)) == 0 && year.remainder(java.math.BigInteger.valueOf(400)).compareTo(java.math.BigInteger.valueOf(0)) != 0) ? DOOMSDAY_NOT_LEAP[_idx((DOOMSDAY_NOT_LEAP).length, ((java.math.BigInteger)(month.subtract(java.math.BigInteger.valueOf(1)))).longValue())] : DOOMSDAY_LEAP[_idx((DOOMSDAY_LEAP).length, ((java.math.BigInteger)(month.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
        java.math.BigInteger week_day_1 = new java.math.BigInteger(String.valueOf((dooms_day_1.add(day).subtract(day_anchor_1)).remainder(java.math.BigInteger.valueOf(7))));
        if (week_day_1.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            week_day_1 = new java.math.BigInteger(String.valueOf(week_day_1.add(java.math.BigInteger.valueOf(7))));
        }
        return ((String)(WEEK_DAY_NAMES).get(week_day_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            WEEK_DAY_NAMES = ((java.util.Map<java.math.BigInteger,String>)(new java.util.LinkedHashMap<java.math.BigInteger, String>() {{ put(java.math.BigInteger.valueOf(0), "Sunday"); put(java.math.BigInteger.valueOf(1), "Monday"); put(java.math.BigInteger.valueOf(2), "Tuesday"); put(java.math.BigInteger.valueOf(3), "Wednesday"); put(java.math.BigInteger.valueOf(4), "Thursday"); put(java.math.BigInteger.valueOf(5), "Friday"); put(java.math.BigInteger.valueOf(6), "Saturday"); }}));
            System.out.println(get_week_day(java.math.BigInteger.valueOf(2020), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(24)));
            System.out.println(get_week_day(java.math.BigInteger.valueOf(2017), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(24)));
            System.out.println(get_week_day(java.math.BigInteger.valueOf(2019), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3)));
            System.out.println(get_week_day(java.math.BigInteger.valueOf(1970), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(16)));
            System.out.println(get_week_day(java.math.BigInteger.valueOf(1870), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(13)));
            System.out.println(get_week_day(java.math.BigInteger.valueOf(2040), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(14)));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
