public class Main {

    static int index_of(String s, String c) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(c))) {
                return i;
            }
            i = i + 1;
        }
        return (-1);
    }

    static String atbash(String sequence) {
        String lower = "abcdefghijklmnopqrstuvwxyz";
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_rev = "zyxwvutsrqponmlkjihgfedcba";
        String upper_rev = "ZYXWVUTSRQPONMLKJIHGFEDCBA";
        String result = "";
        int i_1 = 0;
        while (i_1 < _runeLen(sequence)) {
            String ch = sequence.substring(i_1, i_1+1);
            int idx = index_of(lower, ch);
            if (idx != (-1)) {
                result = result + lower_rev.substring(idx, idx+1);
            } else {
                int idx2 = index_of(upper, ch);
                if (idx2 != (-1)) {
                    result = result + upper_rev.substring(idx2, idx2+1);
                } else {
                    result = result + ch;
                }
            }
            i_1 = i_1 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        System.out.println(atbash("ABCDEFGH"));
        System.out.println(atbash("123GGjj"));
        System.out.println(atbash("testStringtest"));
        System.out.println(atbash("with space"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
