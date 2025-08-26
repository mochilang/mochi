public class Main {
    static java.math.BigInteger[] p4_table = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1)}));
    static String key = "1010000010";
    static String message = "11010111";
    static java.math.BigInteger[] p8_table = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(9)}));
    static java.math.BigInteger[] p10_table = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(6)}));
    static java.math.BigInteger[] IP = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(7)}));
    static java.math.BigInteger[] IP_inv = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(6)}));
    static java.math.BigInteger[] expansion = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(1)}));
    static java.math.BigInteger[][] s0 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2)}))}));
    static java.math.BigInteger[][] s1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(3)}))}));
    static String temp_2 = null;
    static String left_1 = null;
    static String right_2 = null;
    static String key1;
    static String key2;
    static String CT;
    static String PT;

    static String apply_table(String inp, java.math.BigInteger[] table) {
        String res = "";
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(table.length))) < 0) {
            java.math.BigInteger idx_1 = table[_idx((table).length, ((java.math.BigInteger)(i_1)).longValue())].subtract(java.math.BigInteger.valueOf(1));
            if (idx_1.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
                idx_1 = new java.math.BigInteger(String.valueOf(_runeLen(inp))).subtract(java.math.BigInteger.valueOf(1));
            }
            res = res + _substr(inp, (int)(((java.math.BigInteger)(idx_1)).longValue()), (int)(((java.math.BigInteger)(idx_1.add(java.math.BigInteger.valueOf(1)))).longValue()));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return res;
    }

    static String left_shift(String data) {
        return _substr(data, (int)(1L), (int)((long)(_runeLen(data)))) + _substr(data, (int)(0L), (int)(1L));
    }

    static String xor(String a, String b) {
        String res_1 = "";
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(a)))) < 0 && i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(b)))) < 0) {
            if ((_substr(a, (int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())).equals(_substr(b, (int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue()))))) {
                res_1 = res_1 + "0";
            } else {
                res_1 = res_1 + "1";
            }
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        return res_1;
    }

    static String int_to_binary(java.math.BigInteger n) {
        if (n.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return "0";
        }
        String res_3 = "";
        java.math.BigInteger num_1 = n;
        while (num_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            res_3 = _p(num_1.remainder(java.math.BigInteger.valueOf(2))) + res_3;
            num_1 = num_1.divide(java.math.BigInteger.valueOf(2));
        }
        return res_3;
    }

    static String pad_left(String s, java.math.BigInteger width) {
        String res_4 = s;
        while (new java.math.BigInteger(String.valueOf(_runeLen(res_4))).compareTo(width) < 0) {
            res_4 = "0" + res_4;
        }
        return res_4;
    }

    static java.math.BigInteger bin_to_int(String s) {
        java.math.BigInteger result = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            java.math.BigInteger digit_1 = new java.math.BigInteger(String.valueOf(Integer.parseInt(_substr(s, (int)(((java.math.BigInteger)(i_5)).longValue()), (int)(((java.math.BigInteger)(i_5.add(java.math.BigInteger.valueOf(1)))).longValue())))));
            result = result.multiply(java.math.BigInteger.valueOf(2)).add(digit_1);
            i_5 = i_5.add(java.math.BigInteger.valueOf(1));
        }
        return result;
    }

    static String apply_sbox(java.math.BigInteger[][] s, String data) {
        String row_bits = _substr(data, (int)(0L), (int)(1L)) + _substr(data, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(data))).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(_runeLen(data))));
        String col_bits_1 = _substr(data, (int)(1L), (int)(3L));
        java.math.BigInteger row_1 = bin_to_int(row_bits);
        java.math.BigInteger col_1 = bin_to_int(col_bits_1);
        java.math.BigInteger val_1 = s[_idx((s).length, ((java.math.BigInteger)(row_1)).longValue())][_idx((s[_idx((s).length, ((java.math.BigInteger)(row_1)).longValue())]).length, ((java.math.BigInteger)(col_1)).longValue())];
        String out_1 = String.valueOf(int_to_binary(val_1));
        return out_1;
    }

    static String f(java.math.BigInteger[] expansion, java.math.BigInteger[][] s0, java.math.BigInteger[][] s1, String key, String message) {
        String left = _substr(message, (int)(0L), (int)(4L));
        String right_1 = _substr(message, (int)(4L), (int)(8L));
        String temp_1 = String.valueOf(apply_table(right_1, ((java.math.BigInteger[])(expansion))));
        temp_1 = String.valueOf(xor(temp_1, key));
        String left_bin_str_1 = String.valueOf(apply_sbox(((java.math.BigInteger[][])(s0)), _substr(temp_1, (int)(0L), (int)(4L))));
        String right_bin_str_1 = String.valueOf(apply_sbox(((java.math.BigInteger[][])(s1)), _substr(temp_1, (int)(4L), (int)(8L))));
        left_bin_str_1 = String.valueOf(pad_left(left_bin_str_1, java.math.BigInteger.valueOf(2)));
        right_bin_str_1 = String.valueOf(pad_left(right_bin_str_1, java.math.BigInteger.valueOf(2)));
        temp_1 = String.valueOf(apply_table(left_bin_str_1 + right_bin_str_1, ((java.math.BigInteger[])(p4_table))));
        temp_1 = String.valueOf(xor(left, temp_1));
        return temp_1 + right_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            temp_2 = String.valueOf(apply_table(key, ((java.math.BigInteger[])(p10_table))));
            left_1 = _substr(temp_2, (int)(0L), (int)(5L));
            right_2 = _substr(temp_2, (int)(5L), (int)(10L));
            left_1 = String.valueOf(left_shift(left_1));
            right_2 = String.valueOf(left_shift(right_2));
            key1 = String.valueOf(apply_table(left_1 + right_2, ((java.math.BigInteger[])(p8_table))));
            left_1 = String.valueOf(left_shift(left_1));
            right_2 = String.valueOf(left_shift(right_2));
            left_1 = String.valueOf(left_shift(left_1));
            right_2 = String.valueOf(left_shift(right_2));
            key2 = String.valueOf(apply_table(left_1 + right_2, ((java.math.BigInteger[])(p8_table))));
            temp_2 = String.valueOf(apply_table(message, ((java.math.BigInteger[])(IP))));
            temp_2 = String.valueOf(f(((java.math.BigInteger[])(expansion)), ((java.math.BigInteger[][])(s0)), ((java.math.BigInteger[][])(s1)), key1, temp_2));
            temp_2 = _substr(temp_2, (int)(4L), (int)(8L)) + _substr(temp_2, (int)(0L), (int)(4L));
            temp_2 = String.valueOf(f(((java.math.BigInteger[])(expansion)), ((java.math.BigInteger[][])(s0)), ((java.math.BigInteger[][])(s1)), key2, temp_2));
            CT = String.valueOf(apply_table(temp_2, ((java.math.BigInteger[])(IP_inv))));
            System.out.println("Cipher text is: " + CT);
            temp_2 = String.valueOf(apply_table(CT, ((java.math.BigInteger[])(IP))));
            temp_2 = String.valueOf(f(((java.math.BigInteger[])(expansion)), ((java.math.BigInteger[][])(s0)), ((java.math.BigInteger[][])(s1)), key2, temp_2));
            temp_2 = _substr(temp_2, (int)(4L), (int)(8L)) + _substr(temp_2, (int)(0L), (int)(4L));
            temp_2 = String.valueOf(f(((java.math.BigInteger[])(expansion)), ((java.math.BigInteger[][])(s0)), ((java.math.BigInteger[][])(s1)), key1, temp_2));
            PT = String.valueOf(apply_table(temp_2, ((java.math.BigInteger[])(IP_inv))));
            System.out.println("Plain text after decypting is: " + PT);
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
