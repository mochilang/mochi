public class Main {

    static String bin_to_octal(String bin_string) {
        int i = 0;
        while (i < _runeLen(bin_string)) {
            String c = bin_string.substring(i, i+1);
            if (!((c.equals("0")) || (c.equals("1")))) {
                throw new RuntimeException(String.valueOf("Non-binary value was passed to the function"));
            }
            i = i + 1;
        }
        if (_runeLen(bin_string) == 0) {
            throw new RuntimeException(String.valueOf("Empty string was passed to the function"));
        }
        String padded = bin_string;
        while (Math.floorMod(_runeLen(padded), 3) != 0) {
            padded = "0" + padded;
        }
        String oct_string = "";
        int index = 0;
        while (index < _runeLen(padded)) {
            String group = padded.substring(index, index + 3);
            int b0 = (group.substring(0, 0+1).equals("1")) ? 1 : 0;
            int b1 = (group.substring(1, 1+1).equals("1")) ? 1 : 0;
            int b2 = (group.substring(2, 2+1).equals("1")) ? 1 : 0;
            int oct_val = b0 * 4 + b1 * 2 + b2;
            oct_string = oct_string + _p(oct_val);
            index = index + 3;
        }
        return oct_string;
    }
    public static void main(String[] args) {
        System.out.println(bin_to_octal("1111"));
        System.out.println(bin_to_octal("101010101010011"));
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
