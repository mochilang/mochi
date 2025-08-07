public class Main {
    static String[] logins1;
    static String[] logins2;

    static int parse_int(String s) {
        int value = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i+1);
            value = value * 10 + (Integer.parseInt(c));
            i = i + 1;
        }
        return value;
    }

    static String join(String[] xs) {
        String s = "";
        int i_1 = 0;
        while (i_1 < xs.length) {
            s = s + xs[i_1];
            i_1 = i_1 + 1;
        }
        return s;
    }

    static boolean contains(String[] xs, String c) {
        int i_2 = 0;
        while (i_2 < xs.length) {
            if ((xs[i_2].equals(c))) {
                return true;
            }
            i_2 = i_2 + 1;
        }
        return false;
    }

    static int index_of(String[] xs, String c) {
        int i_3 = 0;
        while (i_3 < xs.length) {
            if ((xs[i_3].equals(c))) {
                return i_3;
            }
            i_3 = i_3 + 1;
        }
        return -1;
    }

    static String[] remove_at(String[] xs, int idx) {
        String[] res = ((String[])(new String[]{}));
        int i_4 = 0;
        while (i_4 < xs.length) {
            if (i_4 != idx) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[i_4])).toArray(String[]::new)));
            }
            i_4 = i_4 + 1;
        }
        return res;
    }

    static String[] unique_chars(String[] logins) {
        String[] chars = ((String[])(new String[]{}));
        int i_5 = 0;
        while (i_5 < logins.length) {
            String login = logins[i_5];
            int j = 0;
            while (j < _runeLen(login)) {
                String c_1 = login.substring(j, j+1);
                if (!(Boolean)contains(((String[])(chars)), c_1)) {
                    chars = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(c_1)).toArray(String[]::new)));
                }
                j = j + 1;
            }
            i_5 = i_5 + 1;
        }
        return chars;
    }

    static boolean satisfies(String[] permutation, String[] logins) {
        int i_6 = 0;
        while (i_6 < logins.length) {
            String login_1 = logins[i_6];
            int i0 = index_of(((String[])(permutation)), login_1.substring(0, 0+1));
            int i1 = index_of(((String[])(permutation)), login_1.substring(1, 1+1));
            int i2 = index_of(((String[])(permutation)), login_1.substring(2, 2+1));
            if (!(i0 < i1 && i1 < i2)) {
                return false;
            }
            i_6 = i_6 + 1;
        }
        return true;
    }

    static String search(String[] chars, String[] current, String[] logins) {
        if (chars.length == 0) {
            if (((Boolean)(satisfies(((String[])(current)), ((String[])(logins)))))) {
                return join(((String[])(current)));
            }
            return "";
        }
        int i_7 = 0;
        while (i_7 < chars.length) {
            String c_2 = chars[i_7];
            String[] rest = ((String[])(remove_at(((String[])(chars)), i_7)));
            String[] next = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(current), java.util.stream.Stream.of(c_2)).toArray(String[]::new)));
            String res_1 = String.valueOf(search(((String[])(rest)), ((String[])(next)), ((String[])(logins))));
            if (!(res_1.equals(""))) {
                return res_1;
            }
            i_7 = i_7 + 1;
        }
        return "";
    }

    static int find_secret_passcode(String[] logins) {
        String[] chars_1 = ((String[])(unique_chars(((String[])(logins)))));
        String s_1 = String.valueOf(search(((String[])(chars_1)), ((String[])(new String[]{})), ((String[])(logins))));
        if ((s_1.equals(""))) {
            return -1;
        }
        return parse_int(s_1);
    }
    public static void main(String[] args) {
        logins1 = ((String[])(new String[]{"135", "259", "235", "189", "690", "168", "120", "136", "289", "589", "160", "165", "580", "369", "250", "280"}));
        System.out.println(_p(find_secret_passcode(((String[])(logins1)))));
        logins2 = ((String[])(new String[]{"426", "281", "061", "819", "268", "406", "420", "428", "209", "689", "019", "421", "469", "261", "681", "201"}));
        System.out.println(_p(find_secret_passcode(((String[])(logins2)))));
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
}
