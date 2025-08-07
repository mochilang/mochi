public class Main {
    static String ascii_chars;
    static int[] LOWERCASE_INTS = new int[0];
    static int i_1 = 0;
    static String[] COMMON_WORDS = new String[0];
    static int[] ciphertext;

    static int xor(int a, int b) {
        int res = 0;
        int bit = 1;
        int x = a;
        int y = b;
        while (x > 0 || y > 0) {
            int abit = Math.floorMod(x, 2);
            int bbit = Math.floorMod(y, 2);
            if (abit != bbit) {
                res = res + bit;
            }
            x = Math.floorDiv(x, 2);
            y = Math.floorDiv(y, 2);
            bit = bit * 2;
        }
        return res;
    }

    static String chr(int code) {
        if (code == 10) {
            return "\n";
        }
        if (code == 13) {
            return "\r";
        }
        if (code == 9) {
            return "\t";
        }
        if (code >= 32 && code < 127) {
            return ascii_chars.substring(code - 32, code - 31);
        }
        return "";
    }

    static int ord(String ch) {
        if ((ch.equals("\n"))) {
            return 10;
        }
        if ((ch.equals("\r"))) {
            return 13;
        }
        if ((ch.equals("\t"))) {
            return 9;
        }
        int i = 0;
        while (i < _runeLen(ascii_chars)) {
            if ((ascii_chars.substring(i, i + 1).equals(ch))) {
                return 32 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static boolean is_valid_ascii(int code) {
        if (code >= 32 && code <= 126) {
            return true;
        }
        if (code == 9 || code == 10 || code == 13) {
            return true;
        }
        return false;
    }

    static String try_key(int[] ciphertext, int[] key) {
        String decoded = "";
        int i_2 = 0;
        int klen = key.length;
        while (i_2 < ciphertext.length) {
            int decodedchar = xor(ciphertext[i_2], key[Math.floorMod(i_2, klen)]);
            if (!(Boolean)is_valid_ascii(decodedchar)) {
                return null;
            }
            decoded = decoded + String.valueOf(chr(decodedchar));
            i_2 = i_2 + 1;
        }
        return decoded;
    }

    static String[] filter_valid_chars(int[] ciphertext) {
        String[] possibles = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < LOWERCASE_INTS.length) {
            int j = 0;
            while (j < LOWERCASE_INTS.length) {
                int k = 0;
                while (k < LOWERCASE_INTS.length) {
                    int[] key = ((int[])(new int[]{LOWERCASE_INTS[i_3], LOWERCASE_INTS[j], LOWERCASE_INTS[k]}));
                    String decoded_1 = String.valueOf(try_key(((int[])(ciphertext)), ((int[])(key))));
                    if (!(decoded_1 == null)) {
                        possibles = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(possibles), java.util.stream.Stream.of(decoded_1)).toArray(String[]::new)));
                    }
                    k = k + 1;
                }
                j = j + 1;
            }
            i_3 = i_3 + 1;
        }
        return possibles;
    }

    static boolean contains(String s, String sub) {
        int n = _runeLen(s);
        int m = _runeLen(sub);
        if (m == 0) {
            return true;
        }
        int i_4 = 0;
        while (i_4 <= n - m) {
            int j_1 = 0;
            boolean is_match = true;
            while (j_1 < m) {
                if (!(s.substring(i_4 + j_1, i_4 + j_1+1).equals(sub.substring(j_1, j_1+1)))) {
                    is_match = false;
                    break;
                }
                j_1 = j_1 + 1;
            }
            if (is_match) {
                return true;
            }
            i_4 = i_4 + 1;
        }
        return false;
    }

    static String[] filter_common_word(String[] possibles, String common_word) {
        String[] res_1 = ((String[])(new String[]{}));
        int i_5 = 0;
        while (i_5 < possibles.length) {
            String p = possibles[i_5];
            if (((Boolean)(contains(p.toLowerCase(), common_word)))) {
                res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(p)).toArray(String[]::new)));
            }
            i_5 = i_5 + 1;
        }
        return res_1;
    }

    static int solution(int[] ciphertext) {
        String[] possibles_1 = ((String[])(filter_valid_chars(((int[])(ciphertext)))));
        int i_6 = 0;
        while (i_6 < COMMON_WORDS.length) {
            String word = COMMON_WORDS[i_6];
            possibles_1 = ((String[])(filter_common_word(((String[])(possibles_1)), word)));
            if (possibles_1.length == 1) {
                break;
            }
            i_6 = i_6 + 1;
        }
        String decoded_text = possibles_1[0];
        int sum = 0;
        int j_2 = 0;
        while (j_2 < _runeLen(decoded_text)) {
            sum = sum + ord(_substr(decoded_text, j_2, j_2 + 1));
            j_2 = j_2 + 1;
        }
        return sum;
    }
    public static void main(String[] args) {
        ascii_chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
        LOWERCASE_INTS = ((int[])(new int[]{}));
        i_1 = 97;
        while (i_1 <= 122) {
            LOWERCASE_INTS = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(LOWERCASE_INTS), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        COMMON_WORDS = ((String[])(new String[]{"the", "be", "to", "of", "and", "in", "that", "have"}));
        ciphertext = ((int[])(new int[]{17, 6, 1, 69, 12, 1, 69, 26, 11, 69, 1, 2, 69, 15, 10, 1, 78, 13, 11, 78, 16, 13, 15, 16, 69, 6, 5, 19, 11}));
        System.out.println(_p(solution(((int[])(ciphertext)))));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
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
        return String.valueOf(v);
    }
}
