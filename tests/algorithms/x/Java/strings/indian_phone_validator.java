public class Main {

    static boolean all_digits(String s) {
        if (_runeLen(s) == 0) {
            return false;
        }
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i+1);
            if ((c.compareTo("0") < 0) || (c.compareTo("9") > 0)) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static boolean indian_phone_validator(String phone) {
        String s = phone;
        if (_runeLen(s) >= 3 && (_substr(s, 0, 3).equals("+91"))) {
            s = _substr(s, 3, _runeLen(s));
            if (_runeLen(s) > 0) {
                String c_1 = s.substring(0, 0+1);
                if ((c_1.equals("-")) || (c_1.equals(" "))) {
                    s = _substr(s, 1, _runeLen(s));
                }
            }
        }
        if (_runeLen(s) > 0 && (s.substring(0, 0+1).equals("0"))) {
            s = _substr(s, 1, _runeLen(s));
        }
        if (_runeLen(s) >= 2 && (_substr(s, 0, 2).equals("91"))) {
            s = _substr(s, 2, _runeLen(s));
        }
        if (_runeLen(s) != 10) {
            return false;
        }
        String first = s.substring(0, 0+1);
        if (!((first.equals("7")) || (first.equals("8")) || (first.equals("9")))) {
            return false;
        }
        if (!(Boolean)all_digits(s)) {
            return false;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(indian_phone_validator("+91123456789")));
            System.out.println(_p(indian_phone_validator("+919876543210")));
            System.out.println(_p(indian_phone_validator("01234567896")));
            System.out.println(_p(indian_phone_validator("919876543218")));
            System.out.println(_p(indian_phone_validator("+91-1234567899")));
            System.out.println(_p(indian_phone_validator("+91-9876543218")));
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
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
