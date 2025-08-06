public class Main {
    static String LETTERS;

    static int index_of(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return 0 - 1;
    }

    static void decrypt(String message) {
        for (int key = 0; key < _runeLen(LETTERS); key++) {
            String translated = "";
            for (int i = 0; i < _runeLen(message); i++) {
                String symbol = _substr(message, i, i + 1);
                int idx = index_of(LETTERS, symbol);
                if (idx != 0 - 1) {
                    int num = idx - key;
                    if (num < 0) {
                        num = num + _runeLen(LETTERS);
                    }
                    translated = translated + _substr(LETTERS, num, num + 1);
                } else {
                    translated = translated + symbol;
                }
            }
            System.out.println("Decryption using Key #" + _p(key) + ": " + translated);
        }
    }
    public static void main(String[] args) {
        LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        decrypt("TMDETUX PMDVU");
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
