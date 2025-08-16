public class Main {
    static long[] p4_table = ((long[])(new long[]{2, 4, 3, 1}));
    static String key = "1010000010";
    static String message = "11010111";
    static long[] p8_table = ((long[])(new long[]{6, 3, 7, 4, 8, 5, 10, 9}));
    static long[] p10_table = ((long[])(new long[]{3, 5, 2, 7, 4, 10, 1, 9, 8, 6}));
    static long[] IP = ((long[])(new long[]{2, 6, 3, 1, 4, 8, 5, 7}));
    static long[] IP_inv = ((long[])(new long[]{4, 1, 3, 5, 7, 2, 8, 6}));
    static long[] expansion = ((long[])(new long[]{4, 1, 2, 3, 2, 3, 4, 1}));
    static long[][] s0 = ((long[][])(new long[][]{new long[]{1, 0, 3, 2}, new long[]{3, 2, 1, 0}, new long[]{0, 2, 1, 3}, new long[]{3, 1, 3, 2}}));
    static long[][] s1 = ((long[][])(new long[][]{new long[]{0, 1, 2, 3}, new long[]{2, 0, 1, 3}, new long[]{3, 0, 1, 0}, new long[]{2, 1, 0, 3}}));
    static String temp_2 = null;
    static String left_1 = null;
    static String right_2 = null;
    static String key1;
    static String key2;
    static String CT;
    static String PT;

    static String apply_table(String inp, long[] table) {
        String res = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(table.length)) {
            long idx_1 = (long)((long)(table[(int)((long)(i_1))]) - 1L);
            if ((long)(idx_1) < 0L) {
                idx_1 = (long)((long)(_runeLen(inp)) - 1L);
            }
            res = res + _substr(inp, (int)((long)(idx_1)), (int)((long)((long)(idx_1) + 1L))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static String left_shift(String data) {
        return _substr(data, (int)(1L), (int)((long)(_runeLen(data))))) + _substr(data, (int)(0L), (int)(1L)));
    }

    static String xor(String a, String b) {
        String res_1 = "";
        long i_3 = 0L;
        while ((long)(i_3) < (long)(_runeLen(a)) && (long)(i_3) < (long)(_runeLen(b))) {
            if ((_substr(a, (int)((long)(i_3)), (int)((long)((long)(i_3) + 1L)))).equals(_substr(b, (int)((long)(i_3)), (int)((long)((long)(i_3) + 1L))))))) {
                res_1 = res_1 + "0";
            } else {
                res_1 = res_1 + "1";
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res_1;
    }

    static String int_to_binary(long n) {
        if ((long)(n) == 0L) {
            return "0";
        }
        String res_3 = "";
        long num_1 = (long)(n);
        while ((long)(num_1) > 0L) {
            res_3 = _p(Math.floorMod(num_1, 2)) + res_3;
            num_1 = (long)((long)(num_1) / 2L);
        }
        return res_3;
    }

    static String pad_left(String s, long width) {
        String res_4 = s;
        while ((long)(_runeLen(res_4)) < (long)(width)) {
            res_4 = "0" + res_4;
        }
        return res_4;
    }

    static long bin_to_int(String s) {
        long result = 0L;
        long i_5 = 0L;
        while ((long)(i_5) < (long)(_runeLen(s))) {
            long digit_1 = (long)(Integer.parseInt(_substr(s, (int)((long)(i_5)), (int)((long)((long)(i_5) + 1L))))));
            result = (long)((long)((long)(result) * 2L) + (long)(digit_1));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return result;
    }

    static String apply_sbox(long[][] s, String data) {
        String row_bits = _substr(data, (int)(0L), (int)(1L))) + _substr(data, (int)((long)((long)(_runeLen(data)) - 1L)), (int)((long)(_runeLen(data)))));
        String col_bits_1 = _substr(data, (int)(1L), (int)(3L)));
        long row_1 = (long)(bin_to_int(row_bits));
        long col_1 = (long)(bin_to_int(col_bits_1));
        long val_1 = (long)(s[(int)((long)(row_1))][(int)((long)(col_1))]);
        String out_1 = String.valueOf(int_to_binary((long)(val_1)));
        return out_1;
    }

    static String f(long[] expansion, long[][] s0, long[][] s1, String key, String message) {
        String left = _substr(message, (int)(0L), (int)(4L)));
        String right_1 = _substr(message, (int)(4L), (int)(8L)));
        String temp_1 = String.valueOf(apply_table(right_1, ((long[])(expansion))));
        temp_1 = String.valueOf(xor(temp_1, key));
        String left_bin_str_1 = String.valueOf(apply_sbox(((long[][])(s0)), _substr(temp_1, (int)(0L), (int)(4L)))));
        String right_bin_str_1 = String.valueOf(apply_sbox(((long[][])(s1)), _substr(temp_1, (int)(4L), (int)(8L)))));
        left_bin_str_1 = String.valueOf(pad_left(left_bin_str_1, 2L));
        right_bin_str_1 = String.valueOf(pad_left(right_bin_str_1, 2L));
        temp_1 = String.valueOf(apply_table(left_bin_str_1 + right_bin_str_1, ((long[])(p4_table))));
        temp_1 = String.valueOf(xor(left, temp_1));
        return temp_1 + right_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            temp_2 = String.valueOf(apply_table(key, ((long[])(p10_table))));
            left_1 = _substr(temp_2, (int)(0L), (int)(5L)));
            right_2 = _substr(temp_2, (int)(5L), (int)(10L)));
            left_1 = String.valueOf(left_shift(left_1));
            right_2 = String.valueOf(left_shift(right_2));
            key1 = String.valueOf(apply_table(left_1 + right_2, ((long[])(p8_table))));
            left_1 = String.valueOf(left_shift(left_1));
            right_2 = String.valueOf(left_shift(right_2));
            left_1 = String.valueOf(left_shift(left_1));
            right_2 = String.valueOf(left_shift(right_2));
            key2 = String.valueOf(apply_table(left_1 + right_2, ((long[])(p8_table))));
            temp_2 = String.valueOf(apply_table(message, ((long[])(IP))));
            temp_2 = String.valueOf(f(((long[])(expansion)), ((long[][])(s0)), ((long[][])(s1)), key1, temp_2));
            temp_2 = _substr(temp_2, (int)(4L), (int)(8L))) + _substr(temp_2, (int)(0L), (int)(4L)));
            temp_2 = String.valueOf(f(((long[])(expansion)), ((long[][])(s0)), ((long[][])(s1)), key2, temp_2));
            CT = String.valueOf(apply_table(temp_2, ((long[])(IP_inv))));
            System.out.println("Cipher text is: " + CT);
            temp_2 = String.valueOf(apply_table(CT, ((long[])(IP))));
            temp_2 = String.valueOf(f(((long[])(expansion)), ((long[][])(s0)), ((long[][])(s1)), key2, temp_2));
            temp_2 = _substr(temp_2, (int)(4L), (int)(8L))) + _substr(temp_2, (int)(0L), (int)(4L)));
            temp_2 = String.valueOf(f(((long[])(expansion)), ((long[][])(s0)), ((long[][])(s1)), key1, temp_2));
            PT = String.valueOf(apply_table(temp_2, ((long[])(IP_inv))));
            System.out.println("Plain text after decypting is: " + PT);
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
