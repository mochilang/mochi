public class Main {
    static String[][] SQUARE;

    static int index_of(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String to_lower_without_spaces(String message, boolean replace_j) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        String res = "";
        int i_1 = 0;
        while (i_1 < _runeLen(message)) {
            String ch = message.substring(i_1, i_1+1);
            int pos = index_of(upper, ch);
            if (pos >= 0) {
                ch = lower.substring(pos, pos+1);
            }
            if (!(ch.equals(" "))) {
                if (((Boolean)(replace_j)) && (ch.equals("j"))) {
                    ch = "i";
                }
                res = res + ch;
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int[] letter_to_numbers(String letter) {
        int r = 0;
        while (r < SQUARE.length) {
            int c = 0;
            while (c < SQUARE[r].length) {
                if ((SQUARE[r][c].equals(letter))) {
                    return ((int[])(new int[]{r + 1, c + 1}));
                }
                c = c + 1;
            }
            r = r + 1;
        }
        return ((int[])(new int[]{0, 0}));
    }

    static String numbers_to_letter(int row, int col) {
        return SQUARE[row - 1][col - 1];
    }

    static String encode(String message) {
        String clean = String.valueOf(to_lower_without_spaces(message, true));
        int l = _runeLen(clean);
        int[] rows = ((int[])(new int[]{}));
        int[] cols = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < l) {
            int[] nums = ((int[])(letter_to_numbers(clean.substring(i_2, i_2+1))));
            rows = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(rows), java.util.stream.IntStream.of(nums[0])).toArray()));
            cols = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(cols), java.util.stream.IntStream.of(nums[1])).toArray()));
            i_2 = i_2 + 1;
        }
        int[] seq = ((int[])(new int[]{}));
        i_2 = 0;
        while (i_2 < l) {
            seq = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(seq), java.util.stream.IntStream.of(rows[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        i_2 = 0;
        while (i_2 < l) {
            seq = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(seq), java.util.stream.IntStream.of(cols[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        String encoded = "";
        i_2 = 0;
        while (i_2 < l) {
            int r_1 = seq[2 * i_2];
            int c_1 = seq[2 * i_2 + 1];
            encoded = encoded + String.valueOf(numbers_to_letter(r_1, c_1));
            i_2 = i_2 + 1;
        }
        return encoded;
    }

    static String decode(String message) {
        String clean_1 = String.valueOf(to_lower_without_spaces(message, false));
        int l_1 = _runeLen(clean_1);
        int[] first = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < l_1) {
            int[] nums_1 = ((int[])(letter_to_numbers(clean_1.substring(i_3, i_3+1))));
            first = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(first), java.util.stream.IntStream.of(nums_1[0])).toArray()));
            first = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(first), java.util.stream.IntStream.of(nums_1[1])).toArray()));
            i_3 = i_3 + 1;
        }
        int[] top = ((int[])(new int[]{}));
        int[] bottom = ((int[])(new int[]{}));
        i_3 = 0;
        while (i_3 < l_1) {
            top = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(top), java.util.stream.IntStream.of(first[i_3])).toArray()));
            bottom = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bottom), java.util.stream.IntStream.of(first[i_3 + l_1])).toArray()));
            i_3 = i_3 + 1;
        }
        String decoded = "";
        i_3 = 0;
        while (i_3 < l_1) {
            int r_2 = top[i_3];
            int c_2 = bottom[i_3];
            decoded = decoded + String.valueOf(numbers_to_letter(r_2, c_2));
            i_3 = i_3 + 1;
        }
        return decoded;
    }
    public static void main(String[] args) {
        SQUARE = ((String[][])(new String[][]{new String[]{"a", "b", "c", "d", "e"}, new String[]{"f", "g", "h", "i", "k"}, new String[]{"l", "m", "n", "o", "p"}, new String[]{"q", "r", "s", "t", "u"}, new String[]{"v", "w", "x", "y", "z"}}));
        System.out.println(encode("testmessage"));
        System.out.println(encode("Test Message"));
        System.out.println(encode("test j"));
        System.out.println(encode("test i"));
        System.out.println(decode("qtltbdxrxlk"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
