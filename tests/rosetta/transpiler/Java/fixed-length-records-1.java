public class Main {
    static String[] records;

    static String repeat(String s, int n) {
        String out = "";
        int i = 0;
        while (i < n) {
            out = out + s;
            i = i + 1;
        }
        return out;
    }

    static String reverseStr(String s) {
        String out_1 = "";
        int i_1 = _runeLen(s) - 1;
        while (i_1 >= 0) {
            out_1 = out_1 + s.substring(i_1, i_1 + 1);
            i_1 = i_1 - 1;
        }
        return out_1;
    }
    public static void main(String[] args) {
        records = new String[]{repeat("abcdefgh", 10), repeat("ijklmnop", 10), repeat("qrstuvwx", 10)};
        for (String r : records) {
            System.out.println(reverseStr(r));
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
