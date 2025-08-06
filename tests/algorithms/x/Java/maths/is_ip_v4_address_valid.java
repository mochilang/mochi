public class Main {

    static String[] split_by_dot(String s) {
        String[] res = ((String[])(new String[]{}));
        String current = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i+1);
            if ((c.equals("."))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + c;
            }
            i = i + 1;
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return res;
    }

    static boolean is_digit_str(String s) {
        if (_runeLen(s) == 0) {
            return false;
        }
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String c_1 = s.substring(i_1, i_1+1);
            if ((c_1.compareTo("0") < 0) || (c_1.compareTo("9") > 0)) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static int parse_decimal(String s) {
        int value = 0;
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            String c_2 = s.substring(i_2, i_2+1);
            value = value * 10 + (Integer.parseInt(c_2));
            i_2 = i_2 + 1;
        }
        return value;
    }

    static boolean is_ip_v4_address_valid(String ip) {
        String[] octets = ((String[])(split_by_dot(ip)));
        if (octets.length != 4) {
            return false;
        }
        int i_3 = 0;
        while (i_3 < 4) {
            String oct = octets[i_3];
            if (!(Boolean)is_digit_str(oct)) {
                return false;
            }
            int number = parse_decimal(oct);
            if (_runeLen(_p(number)) != _runeLen(oct)) {
                return false;
            }
            if (number < 0 || number > 255) {
                return false;
            }
            i_3 = i_3 + 1;
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
        return String.valueOf(v);
    }
}
