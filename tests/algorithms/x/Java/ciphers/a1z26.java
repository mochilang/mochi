public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int charToNum(String ch) {
        String letters = "abcdefghijklmnopqrstuvwxyz";
        int idx = indexOf(letters, ch);
        if (idx >= 0) {
            return idx + 1;
        }
        return 0;
    }

    static String numToChar(int n) {
        String letters_1 = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 1 && n <= 26) {
            return _substr(letters_1, n - 1, n);
        }
        return "?";
    }

    static int[] encode(String plain) {
        int[] res = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < _runeLen(plain)) {
            String ch = _substr(plain, i_1, i_1 + 1).toLowerCase();
            int val = charToNum(ch);
            if (val > 0) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(val)).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String decode(int[] encoded) {
        String out = "";
        for (int n : encoded) {
            out = out + String.valueOf(numToChar(n));
        }
        return out;
    }

    static void main() {
        System.out.println("-> ");
        String text = (_scanner.hasNextLine() ? _scanner.nextLine() : "").toLowerCase();
        int[] enc = ((int[])(encode(text)));
        System.out.println("Encoded: " + _p(enc));
        System.out.println("Decoded: " + String.valueOf(decode(((int[])(enc)))));
    }
    public static void main(String[] args) {
        main();
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
