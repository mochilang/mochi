public class Main {

    static int parse_decimal(String s) {
        int value = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i+1);
            if ((c.compareTo("0") < 0) || (c.compareTo("9") > 0)) {
                throw new RuntimeException(String.valueOf("invalid literal"));
            }
            value = value * 10 + (Integer.parseInt(c));
            i = i + 1;
        }
        return value;
    }

    static String zeller_day(String date_input) {
        java.util.Map<Integer,String> days = ((java.util.Map<Integer,String>)(new java.util.LinkedHashMap<Integer, String>(java.util.Map.ofEntries(java.util.Map.entry(0, "Sunday"), java.util.Map.entry(1, "Monday"), java.util.Map.entry(2, "Tuesday"), java.util.Map.entry(3, "Wednesday"), java.util.Map.entry(4, "Thursday"), java.util.Map.entry(5, "Friday"), java.util.Map.entry(6, "Saturday")))));
        if (_runeLen(date_input) != 10) {
            throw new RuntimeException(String.valueOf("Must be 10 characters long"));
        }
        int m = parse_decimal(date_input.substring(0, 2));
        if (m <= 0 || m >= 13) {
            throw new RuntimeException(String.valueOf("Month must be between 1 - 12"));
        }
        String sep1 = date_input.substring(2, 2+1);
        if (!(sep1.equals("-")) && !(sep1.equals("/"))) {
            throw new RuntimeException(String.valueOf("Date separator must be '-' or '/'"));
        }
        int d = parse_decimal(date_input.substring(3, 5));
        if (d <= 0 || d >= 32) {
            throw new RuntimeException(String.valueOf("Date must be between 1 - 31"));
        }
        String sep2 = date_input.substring(5, 5+1);
        if (!(sep2.equals("-")) && !(sep2.equals("/"))) {
            throw new RuntimeException(String.valueOf("Date separator must be '-' or '/'"));
        }
        int y = parse_decimal(date_input.substring(6, 10));
        if (y <= 45 || y >= 8500) {
            throw new RuntimeException(String.valueOf("Year out of range. There has to be some sort of limit...right?"));
        }
        int year = y;
        int month = m;
        if (month <= 2) {
            year = year - 1;
            month = month + 12;
        }
        int c_1 = Math.floorDiv(year, 100);
        int k = Math.floorMod(year, 100);
        int t = ((Number)(2.6 * (((Number)(month)).doubleValue()) - 5.39)).intValue();
        Object u = Math.floorDiv(c_1, 4);
        int v = Math.floorDiv(k, 4);
        int x = d + k;
        int z = t + ((Number)(u)).intValue() + v + x;
        int w = z - (2 * c_1);
        int f = Math.floorMod(w, 7);
        if (f < 0) {
            f = f + 7;
        }
        return ((String)(days).get(f));
    }

    static String zeller(String date_input) {
        String day = String.valueOf(zeller_day(date_input));
        return "Your date " + date_input + ", is a " + day + "!";
    }

    static void test_zeller() {
        String[] inputs = ((String[])(new String[]{"01-31-2010", "02-01-2010", "11-26-2024", "07-04-1776"}));
        String[] expected = ((String[])(new String[]{"Sunday", "Monday", "Tuesday", "Thursday"}));
        int i_1 = 0;
        while (i_1 < inputs.length) {
            String res = String.valueOf(zeller_day(inputs[i_1]));
            if (!(res.equals(expected[i_1]))) {
                throw new RuntimeException(String.valueOf("zeller test failed"));
            }
            i_1 = i_1 + 1;
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
}
