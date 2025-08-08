public class Main {
    static String lower;
    static String upper;

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

    static String to_title_case(String word) {
        if (_runeLen(word) == 0) {
            return "";
        }
        String first = word.substring(0, 1);
        int idx = index_of(lower, first);
        String result = String.valueOf(idx >= 0 ? upper.substring(idx, idx + 1) : first);
        int i_1 = 1;
        while (i_1 < _runeLen(word)) {
            String ch = word.substring(i_1, i_1 + 1);
            int uidx = index_of(upper, ch);
            if (uidx >= 0) {
                result = result + lower.substring(uidx, uidx + 1);
            } else {
                result = result + ch;
            }
            i_1 = i_1 + 1;
        }
        return result;
    }

    static String[] split_words(String s) {
        String[] words = ((String[])(new String[]{}));
        String current = "";
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            String ch_1 = s.substring(i_2, i_2+1);
            if ((ch_1.equals(" "))) {
                if (_runeLen(current) > 0) {
                    words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                    current = "";
                }
            } else {
                current = current + ch_1;
            }
            i_2 = i_2 + 1;
        }
        if (_runeLen(current) > 0) {
            words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        }
        return words;
    }

    static String sentence_to_title_case(String sentence) {
        String[] words_1 = ((String[])(split_words(sentence)));
        String res = "";
        int i_3 = 0;
        while (i_3 < words_1.length) {
            res = res + String.valueOf(to_title_case(words_1[i_3]));
            if (i_3 + 1 < words_1.length) {
                res = res + " ";
            }
            i_3 = i_3 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        lower = "abcdefghijklmnopqrstuvwxyz";
        upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        System.out.println(to_title_case("Aakash"));
        System.out.println(to_title_case("aakash"));
        System.out.println(to_title_case("AAKASH"));
        System.out.println(to_title_case("aAkAsH"));
        System.out.println(sentence_to_title_case("Aakash Giri"));
        System.out.println(sentence_to_title_case("aakash giri"));
        System.out.println(sentence_to_title_case("AAKASH GIRI"));
        System.out.println(sentence_to_title_case("aAkAsH gIrI"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
