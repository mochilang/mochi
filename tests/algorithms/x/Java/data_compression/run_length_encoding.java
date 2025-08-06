public class Main {
    static String example1;
    static String encoded1;
    static String example2;
    static String encoded2;
    static String example3;
    static String encoded3;

    static String run_length_encode(String text) {
        if (_runeLen(text) == 0) {
            return "";
        }
        String encoded = "";
        int count = 1;
        int i = 0;
        while (i < _runeLen(text)) {
            if (i + 1 < _runeLen(text) && (text.substring(i, i+1).equals(text.substring(i + 1, i + 1+1)))) {
                count = count + 1;
            } else {
                encoded = encoded + text.substring(i, i+1) + _p(count);
                count = 1;
            }
            i = i + 1;
        }
        return encoded;
    }

    static String run_length_decode(String encoded) {
        String res = "";
        int i_1 = 0;
        while (i_1 < _runeLen(encoded)) {
            String ch = encoded.substring(i_1, i_1+1);
            i_1 = i_1 + 1;
            String num_str = "";
            while (i_1 < _runeLen(encoded) && (encoded.substring(i_1, i_1+1).compareTo("0") >= 0) && (encoded.substring(i_1, i_1+1).compareTo("9") <= 0)) {
                num_str = num_str + encoded.substring(i_1, i_1+1);
                i_1 = i_1 + 1;
            }
            int count_1 = Integer.parseInt(num_str);
            int j = 0;
            while (j < count_1) {
                res = res + ch;
                j = j + 1;
            }
        }
        return res;
    }
    public static void main(String[] args) {
        example1 = "AAAABBBCCDAA";
        encoded1 = String.valueOf(run_length_encode(example1));
        System.out.println(encoded1);
        System.out.println(run_length_decode(encoded1));
        example2 = "A";
        encoded2 = String.valueOf(run_length_encode(example2));
        System.out.println(encoded2);
        System.out.println(run_length_decode(encoded2));
        example3 = "AAADDDDDDFFFCCCAAVVVV";
        encoded3 = String.valueOf(run_length_encode(example3));
        System.out.println(encoded3);
        System.out.println(run_length_decode(encoded3));
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
