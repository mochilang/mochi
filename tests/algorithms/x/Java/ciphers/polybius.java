public class Main {
    static String[][] square;

    static int[] letter_to_numbers(String letter) {
        int i = 0;
        while (i < square.length) {
            int j = 0;
            while (j < square[i].length) {
                if ((square[i][j].equals(letter))) {
                    return new int[]{i + 1, j + 1};
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return new int[]{0, 0};
    }

    static String numbers_to_letter(int index1, int index2) {
        return square[index1 - 1][index2 - 1];
    }

    static int char_to_int(String ch) {
        if ((ch.equals("1"))) {
            return 1;
        }
        if ((ch.equals("2"))) {
            return 2;
        }
        if ((ch.equals("3"))) {
            return 3;
        }
        if ((ch.equals("4"))) {
            return 4;
        }
        if ((ch.equals("5"))) {
            return 5;
        }
        return 0;
    }

    static String encode(String message) {
        message = message.toLowerCase();
        String encoded = "";
        int i_1 = 0;
        while (i_1 < _runeLen(message)) {
            String ch = message.substring(i_1, i_1+1);
            if ((ch.equals("j"))) {
                ch = "i";
            }
            if (!(ch.equals(" "))) {
                int[] nums = ((int[])(letter_to_numbers(ch)));
                encoded = encoded + _p(_geti(nums, 0)) + _p(_geti(nums, 1));
            } else {
                encoded = encoded + " ";
            }
            i_1 = i_1 + 1;
        }
        return encoded;
    }

    static String decode(String message) {
        String decoded = "";
        int i_2 = 0;
        while (i_2 < _runeLen(message)) {
            if ((message.substring(i_2, i_2+1).equals(" "))) {
                decoded = decoded + " ";
                i_2 = i_2 + 1;
            } else {
                int index1 = char_to_int(message.substring(i_2, i_2+1));
                int index2 = char_to_int(message.substring(i_2 + 1, i_2 + 1+1));
                String letter = String.valueOf(numbers_to_letter(index1, index2));
                decoded = decoded + letter;
                i_2 = i_2 + 2;
            }
        }
        return decoded;
    }
    public static void main(String[] args) {
        square = ((String[][])(new String[][]{new String[]{"a", "b", "c", "d", "e"}, new String[]{"f", "g", "h", "i", "k"}, new String[]{"l", "m", "n", "o", "p"}, new String[]{"q", "r", "s", "t", "u"}, new String[]{"v", "w", "x", "y", "z"}}));
        System.out.println(encode("test message"));
        System.out.println(encode("Test Message"));
        System.out.println(decode("44154344 32154343112215"));
        System.out.println(decode("4415434432154343112215"));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
