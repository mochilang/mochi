public class Main {
    static String uppercase;
    static String lowercase;

    static int index_of(String s, String c) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(c))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String dencrypt(String s, int n) {
        String out = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            int idx_u = index_of(uppercase, ch);
            if (idx_u >= 0) {
                int new_idx = Math.floorMod((idx_u + n), 26);
                out = out + _substr(uppercase, new_idx, new_idx + 1);
            } else {
                int idx_l = index_of(lowercase, ch);
                if (idx_l >= 0) {
                    int new_idx_1 = Math.floorMod((idx_l + n), 26);
                    out = out + _substr(lowercase, new_idx_1, new_idx_1 + 1);
                } else {
                    out = out + ch;
                }
            }
            i_1 = i_1 + 1;
        }
        return out;
    }

    static void main() {
        String msg = "My secret bank account number is 173-52946 so don't tell anyone!!";
        String s = String.valueOf(dencrypt(msg, 13));
        System.out.println(s);
        System.out.println(_p((dencrypt(s, 13).equals(msg))));
    }
    public static void main(String[] args) {
        uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        lowercase = "abcdefghijklmnopqrstuvwxyz";
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
