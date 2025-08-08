public class Main {

    static int parse_int(String s) {
        int value = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i+1);
            value = value * 10 + (Integer.parseInt(c));
            i = i + 1;
        }
        return value;
    }

    static boolean is_polish_national_id(String id) {
        if (_runeLen(id) == 0) {
            return false;
        }
        if ((_substr(id, 0, 1).equals("-"))) {
            return false;
        }
        int input_int = parse_int(id);
        if (input_int < 10100000 || input_int > (int)99923199999L) {
            return false;
        }
        int month = parse_int(_substr(id, 2, 4));
        if (!((month >= 1 && month <= 12) || (month >= 21 && month <= 32) || (month >= 41 && month <= 52) || (month >= 61 && month <= 72) || (month >= 81 && month <= 92))) {
            return false;
        }
        int day = parse_int(_substr(id, 4, 6));
        if (day < 1 || day > 31) {
            return false;
        }
        int[] multipliers = ((int[])(new int[]{1, 3, 7, 9, 1, 3, 7, 9, 1, 3}));
        int subtotal = 0;
        int i_1 = 0;
        while (i_1 < multipliers.length) {
            int digit = parse_int(_substr(id, i_1, i_1 + 1));
            subtotal = subtotal + Math.floorMod((digit * multipliers[i_1]), 10);
            i_1 = i_1 + 1;
        }
        int checksum = 10 - (Math.floorMod(subtotal, 10));
        return checksum == Math.floorMod(input_int, 10);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(is_polish_national_id("02070803628")));
            System.out.println(_p(is_polish_national_id("02150803629")));
            System.out.println(_p(is_polish_national_id("02075503622")));
            System.out.println(_p(is_polish_national_id("-99012212349")));
            System.out.println(_p(is_polish_national_id("990122123499999")));
            System.out.println(_p(is_polish_national_id("02070803621")));
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
