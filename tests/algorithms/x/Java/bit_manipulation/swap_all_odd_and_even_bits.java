public class Main {

    static String pad_left_num(int n) {
        String s = _p(n);
        while (_runeLen(s) < 5) {
            s = " " + s;
        }
        return s;
    }

    static String to_binary(int n) {
        String sign = "";
        int num = n;
        if (num < 0) {
            sign = "-";
            num = 0 - num;
        }
        String bits = "";
        while (num > 0) {
            bits = _p(Math.floorMod(num, 2)) + bits;
            num = (num - (Math.floorMod(num, 2))) / 2;
        }
        if ((bits.equals(""))) {
            bits = "0";
        }
        int min_width = 8;
        while (_runeLen(bits) < (min_width - _runeLen(sign))) {
            bits = "0" + bits;
        }
        return sign + bits;
    }

    static String show_bits(int before, int after) {
        return String.valueOf(pad_left_num(before)) + ": " + String.valueOf(to_binary(before)) + "\n" + String.valueOf(pad_left_num(after)) + ": " + String.valueOf(to_binary(after));
    }

    static int lshift(int num, int k) {
        int result = num;
        int i = 0;
        while (i < k) {
            result = result * 2;
            i = i + 1;
        }
        return result;
    }

    static int rshift(int num, int k) {
        int result_1 = num;
        int i_1 = 0;
        while (i_1 < k) {
            result_1 = (result_1 - (Math.floorMod(result_1, 2))) / 2;
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static int swap_odd_even_bits(int num) {
        int n = num;
        if (n < 0) {
            n = n + (int)4294967296L;
        }
        int result_2 = 0;
        int i_2 = 0;
        while (i_2 < 32) {
            int bit1 = Math.floorMod(rshift(n, i_2), 2);
            int bit2 = Math.floorMod(rshift(n, i_2 + 1), 2);
            result_2 = result_2 + lshift(bit1, i_2 + 1) + lshift(bit2, i_2);
            i_2 = i_2 + 2;
        }
        return result_2;
    }

    static void main() {
        int[] nums = ((int[])(new int[]{-1, 0, 1, 2, 3, 4, 23, 24}));
        int i_3 = 0;
        while (i_3 < nums.length) {
            int n_1 = nums[i_3];
            System.out.println(show_bits(n_1, swap_odd_even_bits(n_1)));
            System.out.println("");
            i_3 = i_3 + 1;
        }
    }
    public static void main(String[] args) {
        main();
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
