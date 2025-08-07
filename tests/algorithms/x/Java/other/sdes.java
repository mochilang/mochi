public class Main {
    static int[] p4_table;
    static String key;
    static String message;
    static int[] p8_table;
    static int[] p10_table;
    static int[] IP;
    static int[] IP_inv;
    static int[] expansion;
    static int[][] s0;
    static int[][] s1;
    static String temp_1 = null;
    static String left_1 = null;
    static String right_1 = null;
    static String key1;
    static String key2;
    static String CT;
    static String PT;

    static String apply_table(String inp, int[] table) {
        String res = "";
        int i = 0;
        while (i < table.length) {
            int idx = table[i] - 1;
            if (idx < 0) {
                idx = _runeLen(inp) - 1;
            }
            res = res + inp.substring(idx, idx + 1);
            i = i + 1;
        }
        return res;
    }

    static String left_shift(String data) {
        return data.substring(1, _runeLen(data)) + data.substring(0, 1);
    }

    static String xor(String a, String b) {
        String res_1 = "";
        int i_1 = 0;
        while (i_1 < _runeLen(a) && i_1 < _runeLen(b)) {
            if ((a.substring(i_1, i_1 + 1).equals(b.substring(i_1, i_1 + 1)))) {
                res_1 = res_1 + "0";
            } else {
                res_1 = res_1 + "1";
            }
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static String int_to_binary(int n) {
        if (n == 0) {
            return "0";
        }
        String res_2 = "";
        int num = n;
        while (num > 0) {
            res_2 = _p(Math.floorMod(num, 2)) + res_2;
            num = Math.floorDiv(num, 2);
        }
        return res_2;
    }

    static String pad_left(String s, int width) {
        String res_3 = s;
        while (_runeLen(res_3) < width) {
            res_3 = "0" + res_3;
        }
        return res_3;
    }

    static int bin_to_int(String s) {
        int result = 0;
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            int digit = Integer.parseInt(s.substring(i_2, i_2 + 1));
            result = result * 2 + digit;
            i_2 = i_2 + 1;
        }
        return result;
    }

    static String apply_sbox(int[][] s, String data) {
        String row_bits = data.substring(0, 1) + data.substring(_runeLen(data) - 1, _runeLen(data));
        String col_bits = data.substring(1, 3);
        int row = bin_to_int(row_bits);
        int col = bin_to_int(col_bits);
        int val = s[row][col];
        String out = String.valueOf(int_to_binary(val));
        return out;
    }

    static String f(int[] expansion, int[][] s0, int[][] s1, String key, String message) {
        String left = message.substring(0, 4);
        String right = message.substring(4, 8);
        String temp = String.valueOf(apply_table(right, ((int[])(expansion))));
        temp = String.valueOf(xor(temp, key));
        String left_bin_str = String.valueOf(apply_sbox(((int[][])(s0)), temp.substring(0, 4)));
        String right_bin_str = String.valueOf(apply_sbox(((int[][])(s1)), temp.substring(4, 8)));
        left_bin_str = String.valueOf(pad_left(left_bin_str, 2));
        right_bin_str = String.valueOf(pad_left(right_bin_str, 2));
        temp = String.valueOf(apply_table(left_bin_str + right_bin_str, ((int[])(p4_table))));
        temp = String.valueOf(xor(left, temp));
        return temp + right;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            p4_table = ((int[])(new int[]{2, 4, 3, 1}));
            key = "1010000010";
            message = "11010111";
            p8_table = ((int[])(new int[]{6, 3, 7, 4, 8, 5, 10, 9}));
            p10_table = ((int[])(new int[]{3, 5, 2, 7, 4, 10, 1, 9, 8, 6}));
            IP = ((int[])(new int[]{2, 6, 3, 1, 4, 8, 5, 7}));
            IP_inv = ((int[])(new int[]{4, 1, 3, 5, 7, 2, 8, 6}));
            expansion = ((int[])(new int[]{4, 1, 2, 3, 2, 3, 4, 1}));
            s0 = ((int[][])(new int[][]{new int[]{1, 0, 3, 2}, new int[]{3, 2, 1, 0}, new int[]{0, 2, 1, 3}, new int[]{3, 1, 3, 2}}));
            s1 = ((int[][])(new int[][]{new int[]{0, 1, 2, 3}, new int[]{2, 0, 1, 3}, new int[]{3, 0, 1, 0}, new int[]{2, 1, 0, 3}}));
            temp_1 = String.valueOf(apply_table(key, ((int[])(p10_table))));
            left_1 = temp_1.substring(0, 5);
            right_1 = temp_1.substring(5, 10);
            left_1 = String.valueOf(left_shift(left_1));
            right_1 = String.valueOf(left_shift(right_1));
            key1 = String.valueOf(apply_table(left_1 + right_1, ((int[])(p8_table))));
            left_1 = String.valueOf(left_shift(left_1));
            right_1 = String.valueOf(left_shift(right_1));
            left_1 = String.valueOf(left_shift(left_1));
            right_1 = String.valueOf(left_shift(right_1));
            key2 = String.valueOf(apply_table(left_1 + right_1, ((int[])(p8_table))));
            temp_1 = String.valueOf(apply_table(message, ((int[])(IP))));
            temp_1 = String.valueOf(f(((int[])(expansion)), ((int[][])(s0)), ((int[][])(s1)), key1, temp_1));
            temp_1 = temp_1.substring(4, 8) + temp_1.substring(0, 4);
            temp_1 = String.valueOf(f(((int[])(expansion)), ((int[][])(s0)), ((int[][])(s1)), key2, temp_1));
            CT = String.valueOf(apply_table(temp_1, ((int[])(IP_inv))));
            System.out.println("Cipher text is: " + CT);
            temp_1 = String.valueOf(apply_table(CT, ((int[])(IP))));
            temp_1 = String.valueOf(f(((int[])(expansion)), ((int[][])(s0)), ((int[][])(s1)), key2, temp_1));
            temp_1 = temp_1.substring(4, 8) + temp_1.substring(0, 4);
            temp_1 = String.valueOf(f(((int[])(expansion)), ((int[][])(s0)), ((int[][])(s1)), key1, temp_1));
            PT = String.valueOf(apply_table(temp_1, ((int[])(IP_inv))));
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
