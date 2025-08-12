public class Main {

    static String[] split_by_dot(String s) {
        String[] res = ((String[])(new String[]{}));
        String current_1 = "";
        long i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String c_1 = s.substring((int)(i_1), (int)(i_1)+1);
            if ((c_1.equals("."))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else {
                current_1 = current_1 + c_1;
            }
            i_1 = i_1 + 1;
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        return res;
    }

    static boolean is_digit_str(String s) {
        if (_runeLen(s) == 0) {
            return false;
        }
        long i_3 = 0;
        while (i_3 < _runeLen(s)) {
            String c_3 = s.substring((int)(i_3), (int)(i_3)+1);
            if ((c_3.compareTo("0") < 0) || (c_3.compareTo("9") > 0)) {
                return false;
            }
            i_3 = i_3 + 1;
        }
        return true;
    }

    static long parse_decimal(String s) {
        long value = 0;
        long i_5 = 0;
        while (i_5 < _runeLen(s)) {
            String c_5 = s.substring((int)(i_5), (int)(i_5)+1);
            value = value * 10 + (Integer.parseInt(c_5));
            i_5 = i_5 + 1;
        }
        return value;
    }

    static boolean is_ip_v4_address_valid(String ip) {
        String[] octets = ((String[])(split_by_dot(ip)));
        if (octets.length != 4) {
            return false;
        }
        long i_7 = 0;
        while (i_7 < 4) {
            String oct_1 = octets[(int)(i_7)];
            if (!(Boolean)is_digit_str(oct_1)) {
                return false;
            }
            long number_1 = parse_decimal(oct_1);
            if (_runeLen(_p(number_1)) != _runeLen(oct_1)) {
                return false;
            }
            if (number_1 < 0 || number_1 > 255) {
                return false;
            }
            i_7 = i_7 + 1;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(is_ip_v4_address_valid("192.168.0.23")));
            System.out.println(_p(is_ip_v4_address_valid("192.256.15.8")));
            System.out.println(_p(is_ip_v4_address_valid("172.100.0.8")));
            System.out.println(_p(is_ip_v4_address_valid("255.256.0.256")));
            System.out.println(_p(is_ip_v4_address_valid("1.2.33333333.4")));
            System.out.println(_p(is_ip_v4_address_valid("1.2.-3.4")));
            System.out.println(_p(is_ip_v4_address_valid("1.2.3")));
            System.out.println(_p(is_ip_v4_address_valid("1.2.3.4.5")));
            System.out.println(_p(is_ip_v4_address_valid("1.2.A.4")));
            System.out.println(_p(is_ip_v4_address_valid("0.0.0.0")));
            System.out.println(_p(is_ip_v4_address_valid("1.2.3.")));
            System.out.println(_p(is_ip_v4_address_valid("1.2.3.05")));
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
