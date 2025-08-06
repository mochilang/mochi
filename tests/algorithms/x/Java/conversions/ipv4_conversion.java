public class Main {
    static String hex_digits;

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

    static int parse_decimal(String s) {
        if (_runeLen(s) == 0) {
            throw new RuntimeException(String.valueOf("Invalid IPv4 address format"));
        }
        int value = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String c_1 = s.substring(i_1, i_1+1);
            if ((c_1.compareTo("0") < 0) || (c_1.compareTo("9") > 0)) {
                throw new RuntimeException(String.valueOf("Invalid IPv4 address format"));
            }
            value = value * 10 + (Integer.parseInt(c_1));
            i_1 = i_1 + 1;
        }
        return value;
    }

    static String to_hex2(int n) {
        int x = n;
        String res_1 = "";
        while (x > 0) {
            int d = Math.floorMod(x, 16);
            res_1 = hex_digits.substring(d, d+1) + res_1;
            x = x / 16;
        }
        while (_runeLen(res_1) < 2) {
            res_1 = "0" + res_1;
        }
        return res_1;
    }

    static int ipv4_to_decimal(String ipv4_address) {
        String[] parts = ((String[])(split_by_dot(ipv4_address)));
        if (parts.length != 4) {
            throw new RuntimeException(String.valueOf("Invalid IPv4 address format"));
        }
        int result = 0;
        int i_2 = 0;
        while (i_2 < 4) {
            int oct = parse_decimal(parts[i_2]);
            if (oct < 0 || oct > 255) {
                throw new RuntimeException(String.valueOf("Invalid IPv4 octet " + _p(oct)));
            }
            result = result * 256 + oct;
            i_2 = i_2 + 1;
        }
        return result;
    }

    static int alt_ipv4_to_decimal(String ipv4_address) {
        String[] parts_1 = ((String[])(split_by_dot(ipv4_address)));
        if (parts_1.length != 4) {
            throw new RuntimeException(String.valueOf("Invalid IPv4 address format"));
        }
        String hex_str = "";
        int i_3 = 0;
        while (i_3 < 4) {
            int oct_1 = parse_decimal(parts_1[i_3]);
            if (oct_1 < 0 || oct_1 > 255) {
                throw new RuntimeException(String.valueOf("Invalid IPv4 octet " + _p(oct_1)));
            }
            hex_str = hex_str + String.valueOf(to_hex2(oct_1));
            i_3 = i_3 + 1;
        }
        int value_1 = 0;
        int k = 0;
        while (k < _runeLen(hex_str)) {
            String c_2 = hex_str.substring(k, k+1);
            int digit = 0 - 1;
            int j = 0;
            while (j < _runeLen(hex_digits)) {
                if ((hex_digits.substring(j, j+1).equals(c_2))) {
                    digit = j;
                }
                j = j + 1;
            }
            if (digit < 0) {
                throw new RuntimeException(String.valueOf("Invalid hex digit"));
            }
            value_1 = value_1 * 16 + digit;
            k = k + 1;
        }
        return value_1;
    }

    static String decimal_to_ipv4(int decimal_ipv4) {
        if (decimal_ipv4 < 0 || decimal_ipv4 > (int)4294967295L) {
            throw new RuntimeException(String.valueOf("Invalid decimal IPv4 address"));
        }
        int n = decimal_ipv4;
        String[] parts_2 = ((String[])(new String[]{}));
        int i_4 = 0;
        while (i_4 < 4) {
            int octet = Math.floorMod(n, 256);
            parts_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts_2), java.util.stream.Stream.of(_p(octet))).toArray(String[]::new)));
            n = n / 256;
            i_4 = i_4 + 1;
        }
        String res_2 = "";
        int j_1 = parts_2.length - 1;
        while (j_1 >= 0) {
            res_2 = res_2 + parts_2[j_1];
            if (j_1 > 0) {
                res_2 = res_2 + ".";
            }
            j_1 = j_1 - 1;
        }
        return res_2;
    }
    public static void main(String[] args) {
        hex_digits = "0123456789abcdef";
        System.out.println(ipv4_to_decimal("192.168.0.1"));
        System.out.println(ipv4_to_decimal("10.0.0.255"));
        System.out.println(alt_ipv4_to_decimal("192.168.0.1"));
        System.out.println(alt_ipv4_to_decimal("10.0.0.255"));
        System.out.println(decimal_to_ipv4((int)3232235521L));
        System.out.println(decimal_to_ipv4(167772415));
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
