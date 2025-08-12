public class Main {

    static long parse_decimal(String s) {
        long value = 0L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(_runeLen(s))) {
            String c_1 = s.substring((int)((long)(i_1)), (int)((long)(i_1))+1);
            if ((c_1.compareTo("0") < 0) || (c_1.compareTo("9") > 0)) {
                throw new RuntimeException(String.valueOf("invalid literal"));
            }
            value = (long)((long)((long)(value) * (long)(10)) + (long)((Integer.parseInt(c_1))));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return value;
    }

    static String zeller_day(String date_input) {
        java.util.Map<Long,String> days = ((java.util.Map<Long,String>)(new java.util.LinkedHashMap<Long, String>(java.util.Map.ofEntries(java.util.Map.entry(0L, "Sunday"), java.util.Map.entry(1L, "Monday"), java.util.Map.entry(2L, "Tuesday"), java.util.Map.entry(3L, "Wednesday"), java.util.Map.entry(4L, "Thursday"), java.util.Map.entry(5L, "Friday"), java.util.Map.entry(6L, "Saturday")))));
        if ((long)(_runeLen(date_input)) != (long)(10)) {
            throw new RuntimeException(String.valueOf("Must be 10 characters long"));
        }
        long m_1 = parse_decimal(_substr(date_input, (int)((long)(0)), (int)((long)(2))));
        if (m_1 <= (long)(0) || m_1 >= (long)(13)) {
            throw new RuntimeException(String.valueOf("Month must be between 1 - 12"));
        }
        String sep1_1 = date_input.substring((int)((long)(2)), (int)((long)(2))+1);
        if (!(sep1_1.equals("-")) && !(sep1_1.equals("/"))) {
            throw new RuntimeException(String.valueOf("Date separator must be '-' or '/'"));
        }
        long d_1 = parse_decimal(_substr(date_input, (int)((long)(3)), (int)((long)(5))));
        if (d_1 <= (long)(0) || d_1 >= (long)(32)) {
            throw new RuntimeException(String.valueOf("Date must be between 1 - 31"));
        }
        String sep2_1 = date_input.substring((int)((long)(5)), (int)((long)(5))+1);
        if (!(sep2_1.equals("-")) && !(sep2_1.equals("/"))) {
            throw new RuntimeException(String.valueOf("Date separator must be '-' or '/'"));
        }
        long y_1 = parse_decimal(_substr(date_input, (int)((long)(6)), (int)((long)(10))));
        if (y_1 <= (long)(45) || y_1 >= (long)(8500)) {
            throw new RuntimeException(String.valueOf("Year out of range. There has to be some sort of limit...right?"));
        }
        long year_1 = y_1;
        long month_1 = m_1;
        if ((long)(month_1) <= (long)(2)) {
            year_1 = (long)((long)(year_1) - (long)(1));
            month_1 = (long)((long)(month_1) + (long)(12));
        }
        long c_3 = Math.floorDiv(year_1, 100);
        long k_1 = Math.floorMod(year_1, 100);
        long t_1 = (long)(((Number)(2.6 * (((Number)(month_1)).doubleValue()) - 5.39)).intValue());
        Object u_1 = Math.floorDiv(c_3, 4);
        long v_1 = Math.floorDiv(k_1, 4);
        long x_1 = (long)(d_1 + (long)(k_1));
        long z_1 = (long)((long)((long)((long)(t_1) + ((Number)(u_1)).intValue()) + (long)(v_1)) + (long)(x_1));
        long w_1 = (long)((long)(z_1) - (long)(((long)(2) * (long)(c_3))));
        long f_1 = Math.floorMod(w_1, 7);
        if ((long)(f_1) < (long)(0)) {
            f_1 = (long)((long)(f_1) + (long)(7));
        }
        return ((String)(days).get(f_1));
    }

    static String zeller(String date_input) {
        String day = String.valueOf(zeller_day(date_input));
        return "Your date " + date_input + ", is a " + day + "!";
    }

    static void test_zeller() {
        String[] inputs = ((String[])(new String[]{"01-31-2010", "02-01-2010", "11-26-2024", "07-04-1776"}));
        String[] expected_1 = ((String[])(new String[]{"Sunday", "Monday", "Tuesday", "Thursday"}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(inputs.length)) {
            String res_1 = String.valueOf(zeller_day(((String)_geto(inputs, (int)((long)(i_3))))));
            if (!(res_1.equals(((String)_geto(expected_1, (int)((long)(i_3))))))) {
                throw new RuntimeException(String.valueOf("zeller test failed"));
            }
            i_3 = (long)((long)(i_3) + (long)(1));
        }
    }

    static void main() {
        test_zeller();
        System.out.println(zeller("01-31-2010"));
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

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
