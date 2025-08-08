public class Main {

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

    static String upper(String word) {
        String lower_chars = "abcdefghijklmnopqrstuvwxyz";
        String upper_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String result = "";
        int i_1 = 0;
        while (i_1 < _runeLen(word)) {
            String c = word.substring(i_1, i_1+1);
            int idx = index_of(lower_chars, c);
            if (idx >= 0) {
                result = result + upper_chars.substring(idx, idx + 1);
            } else {
                result = result + c;
            }
            i_1 = i_1 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        System.out.println("wow".toUpperCase());
        System.out.println("Hello".toUpperCase());
        System.out.println("WHAT".toUpperCase());
        System.out.println("wh[]32".toUpperCase());
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
