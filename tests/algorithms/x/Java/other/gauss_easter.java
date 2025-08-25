public class Main {
    static class EasterDate {
        java.math.BigInteger month;
        java.math.BigInteger day;
        EasterDate(java.math.BigInteger month, java.math.BigInteger day) {
            this.month = month;
            this.day = day;
        }
        EasterDate() {}
        @Override public String toString() {
            return String.format("{'month': %s, 'day': %s}", String.valueOf(month), String.valueOf(day));
        }
    }

    static java.math.BigInteger[] years = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1994), java.math.BigInteger.valueOf(2000), java.math.BigInteger.valueOf(2010), java.math.BigInteger.valueOf(2021), java.math.BigInteger.valueOf(2023), java.math.BigInteger.valueOf(2032), java.math.BigInteger.valueOf(2100)}));
    static java.math.BigInteger i = java.math.BigInteger.valueOf(0);

    static EasterDate gauss_easter(java.math.BigInteger year) {
        java.math.BigInteger metonic_cycle = new java.math.BigInteger(String.valueOf(year.remainder(java.math.BigInteger.valueOf(19))));
        java.math.BigInteger julian_leap_year_1 = new java.math.BigInteger(String.valueOf(year.remainder(java.math.BigInteger.valueOf(4))));
        java.math.BigInteger non_leap_year_1 = new java.math.BigInteger(String.valueOf(year.remainder(java.math.BigInteger.valueOf(7))));
        java.math.BigInteger leap_day_inhibits_1 = new java.math.BigInteger(String.valueOf(year.divide(java.math.BigInteger.valueOf(100))));
        java.math.BigInteger lunar_orbit_correction_1 = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(13).add(java.math.BigInteger.valueOf(8).multiply(leap_day_inhibits_1))).divide(java.math.BigInteger.valueOf(25))));
        double leap_day_reinstall_number_1 = (double)((double)((((Number)(leap_day_inhibits_1)).doubleValue())) / (double)(4.0));
        double secular_moon_shift_1 = (double)((double)(((double)((double)((double)(15.0) - (double)((((Number)(lunar_orbit_correction_1)).doubleValue()))) + (double)((((Number)(leap_day_inhibits_1)).doubleValue()))) - (double)(leap_day_reinstall_number_1))) % (double)(30.0));
        double century_starting_point_1 = (double)((double)(((double)((double)(4.0) + (double)((((Number)(leap_day_inhibits_1)).doubleValue()))) - (double)(leap_day_reinstall_number_1))) % (double)(7.0));
        double days_to_add_1 = (double)((double)(((double)((double)(19.0) * (double)((((Number)(metonic_cycle)).doubleValue()))) + (double)(secular_moon_shift_1))) % (double)(30.0));
        double days_from_phm_to_sunday_1 = (double)((double)(((double)((double)((double)((double)(2.0) * (double)((((Number)(julian_leap_year_1)).doubleValue()))) + (double)((double)(4.0) * (double)((((Number)(non_leap_year_1)).doubleValue())))) + (double)((double)(6.0) * (double)(days_to_add_1))) + (double)(century_starting_point_1))) % (double)(7.0));
        if ((double)(days_to_add_1) == (double)(29.0) && (double)(days_from_phm_to_sunday_1) == (double)(6.0)) {
            return new EasterDate(java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(19));
        }
        if ((double)(days_to_add_1) == (double)(28.0) && (double)(days_from_phm_to_sunday_1) == (double)(6.0)) {
            return new EasterDate(java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(18));
        }
        java.math.BigInteger offset_1 = new java.math.BigInteger(String.valueOf(((Number)(((double)(days_to_add_1) + (double)(days_from_phm_to_sunday_1)))).intValue()));
        java.math.BigInteger total_1 = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(22).add(offset_1)));
        if (total_1.compareTo(java.math.BigInteger.valueOf(31)) > 0) {
            return new EasterDate(java.math.BigInteger.valueOf(4), new java.math.BigInteger(String.valueOf(total_1.subtract(java.math.BigInteger.valueOf(31)))));
        }
        return new EasterDate(java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf(total_1)));
    }

    static String format_date(java.math.BigInteger year, EasterDate d) {
        String month = String.valueOf(d.month.compareTo(java.math.BigInteger.valueOf(10)) < 0 ? "0" + _p(d.month) : _p(d.month));
        String day_1 = String.valueOf(d.day.compareTo(java.math.BigInteger.valueOf(10)) < 0 ? "0" + _p(d.day) : _p(d.day));
        return _p(year) + "-" + month + "-" + day_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (i.compareTo(new java.math.BigInteger(String.valueOf(years.length))) < 0) {
                java.math.BigInteger y = new java.math.BigInteger(String.valueOf(years[_idx((years).length, ((java.math.BigInteger)(i)).longValue())]));
                EasterDate e = gauss_easter(new java.math.BigInteger(String.valueOf(y)));
                System.out.println("Easter in " + _p(y) + " is " + String.valueOf(format_date(new java.math.BigInteger(String.valueOf(y)), e)));
                i = new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1))));
            }
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
