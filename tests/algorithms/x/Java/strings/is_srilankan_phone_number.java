public class Main {
    static String phone;

    static boolean starts_with(String s, String prefix) {
        if (_runeLen(s) < _runeLen(prefix)) {
            return false;
        }
        return (_substr(s, 0, _runeLen(prefix)).equals(prefix));
    }

    static boolean all_digits(String s) {
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

    static boolean is_sri_lankan_phone_number(String phone) {
        String p = phone;
        if (((Boolean)(starts_with(p, "+94")))) {
            p = _substr(p, 3, _runeLen(p));
        } else         if (((Boolean)(starts_with(p, "0094")))) {
            p = _substr(p, 4, _runeLen(p));
        } else         if (((Boolean)(starts_with(p, "94")))) {
            p = _substr(p, 2, _runeLen(p));
        } else         if (((Boolean)(starts_with(p, "0")))) {
            p = _substr(p, 1, _runeLen(p));
        } else {
            return false;
        }
        if (_runeLen(p) != 9 && _runeLen(p) != 10) {
            return false;
        }
        if (!(p.substring(0, 0+1).equals("7"))) {
            return false;
        }
        String second = p.substring(1, 1+1);
        String[] allowed = ((String[])(new String[]{"0", "1", "2", "4", "5", "6", "7", "8"}));
        if (!(java.util.Arrays.asList(allowed).contains(second))) {
            return false;
        }
        int idx = 2;
        if (_runeLen(p) == 10) {
            String sep = p.substring(2, 2+1);
            if (!(sep.equals("-")) && !(sep.equals(" "))) {
                return false;
            }
            idx = 3;
        }
        if (_runeLen(p) - idx != 7) {
            return false;
        }
        String rest = _substr(p, idx, _runeLen(p));
        return all_digits(rest);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            phone = "0094702343221";
            System.out.println(_p(is_sri_lankan_phone_number(phone)));
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
