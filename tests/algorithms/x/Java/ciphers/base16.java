public class Main {
    static int[] example1;
    static int[] example2;

    static String base16_encode(int[] data) {
        String digits = "0123456789ABCDEF";
        String res = "";
        int i = 0;
        while (i < data.length) {
            int b = data[i];
            if (b < 0 || b > 255) {
                throw new RuntimeException(String.valueOf("byte out of range"));
            }
            int hi = b / 16;
            int lo = Math.floorMod(b, 16);
            res = res + digits.substring(hi, hi + 1) + digits.substring(lo, lo + 1);
            i = i + 1;
        }
        return res;
    }

    static int[] base16_decode(String data) {
        String digits_1 = "0123456789ABCDEF";
        if (Math.floorMod(_runeLen(data), 2) != 0) {
            throw new RuntimeException(String.valueOf("Base16 encoded data is invalid: Data does not have an even number of hex digits."));
        }
        java.util.function.Function<String,Integer> hex_value = (ch) -> {
        int j = 0;
        while (j < 16) {
            if ((digits_1.substring(j, j + 1).equals(ch))) {
                return j;
            }
            j = j + 1;
        }
        return -1;
};
        int[] out = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < _runeLen(data)) {
            String hi_char = data.substring(i_1, i_1 + 1);
            String lo_char = data.substring(i_1 + 1, i_1 + 2);
            int hi_1 = hex_value.apply(hi_char);
            int lo_1 = hex_value.apply(lo_char);
            if (hi_1 < 0 || lo_1 < 0) {
                throw new RuntimeException(String.valueOf("Base16 encoded data is invalid: Data is not uppercase hex or it contains invalid characters."));
            }
            out = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(hi_1 * 16 + lo_1)).toArray()));
            i_1 = i_1 + 2;
        }
        return out;
    }
    public static void main(String[] args) {
        example1 = ((int[])(new int[]{72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33}));
        example2 = ((int[])(new int[]{72, 69, 76, 76, 79, 32, 87, 79, 82, 76, 68, 33}));
        System.out.println(base16_encode(((int[])(example1))));
        System.out.println(base16_encode(((int[])(example2))));
        System.out.println(base16_encode(((int[])(new int[]{}))));
        System.out.println(_p(base16_decode("48656C6C6F20576F726C6421")));
        System.out.println(_p(base16_decode("48454C4C4F20574F524C4421")));
        System.out.println(_p(base16_decode("")));
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
