public class Main {
    static String DIGITS;
    static String LOWER;
    static String UPPER;
    static String[] example1 = new String[0];
    static String[] example2 = new String[0];

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

    static boolean is_digit(String ch) {
        return index_of(DIGITS, ch) >= 0;
    }

    static String to_lower(String ch) {
        int idx = index_of(UPPER, ch);
        if (idx >= 0) {
            return LOWER.substring(idx, idx + 1);
        }
        return ch;
    }

    static String pad_left(String s, int width) {
        String res = s;
        while (_runeLen(res) < width) {
            res = "0" + res;
        }
        return res;
    }

    static String[] alphanum_key(String s) {
        String[] key = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if (((Boolean)(is_digit(s.substring(i_1, i_1+1))))) {
                String num = "";
                while (i_1 < _runeLen(s) && ((Boolean)(is_digit(s.substring(i_1, i_1+1))))) {
                    num = num + s.substring(i_1, i_1+1);
                    i_1 = i_1 + 1;
                }
                String len_str = String.valueOf(pad_left(_p(_runeLen(num)), 3));
                key = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(key), java.util.stream.Stream.of("#" + len_str + num)).toArray(String[]::new)));
            } else {
                String seg = "";
                while (i_1 < _runeLen(s)) {
                    if (((Boolean)(is_digit(s.substring(i_1, i_1+1))))) {
                        break;
                    }
                    seg = seg + String.valueOf(to_lower(s.substring(i_1, i_1+1)));
                    i_1 = i_1 + 1;
                }
                key = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(key), java.util.stream.Stream.of(seg)).toArray(String[]::new)));
            }
        }
        return key;
    }

    static int compare_keys(String[] a, String[] b) {
        int i_2 = 0;
        while (i_2 < a.length && i_2 < b.length) {
            if ((a[i_2].compareTo(b[i_2]) < 0)) {
                return -1;
            }
            if ((a[i_2].compareTo(b[i_2]) > 0)) {
                return 1;
            }
            i_2 = i_2 + 1;
        }
        if (a.length < b.length) {
            return -1;
        }
        if (a.length > b.length) {
            return 1;
        }
        return 0;
    }

    static String[] natural_sort(String[] arr) {
        String[] res_1 = ((String[])(new String[]{}));
        String[][] keys = ((String[][])(new String[][]{}));
        int k = 0;
        while (k < arr.length) {
            res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(arr[k])).toArray(String[]::new)));
            keys = ((String[][])(appendObj(keys, alphanum_key(arr[k]))));
            k = k + 1;
        }
        int i_3 = 1;
        while (i_3 < res_1.length) {
            String current = res_1[i_3];
            String[] current_key = ((String[])(keys[i_3]));
            int j = i_3 - 1;
            while (j >= 0 && compare_keys(((String[])(keys[j])), ((String[])(current_key))) > 0) {
res_1[j + 1] = res_1[j];
keys[j + 1] = ((String[])(keys[j]));
                j = j - 1;
            }
res_1[j + 1] = current;
keys[j + 1] = ((String[])(current_key));
            i_3 = i_3 + 1;
        }
        return res_1;
    }
    public static void main(String[] args) {
        DIGITS = "0123456789";
        LOWER = "abcdefghijklmnopqrstuvwxyz";
        UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        example1 = ((String[])(new String[]{"2 ft 7 in", "1 ft 5 in", "10 ft 2 in", "2 ft 11 in", "7 ft 6 in"}));
        System.out.println(_p(natural_sort(((String[])(example1)))));
        example2 = ((String[])(new String[]{"Elm11", "Elm12", "Elm2", "elm0", "elm1", "elm10", "elm13", "elm9"}));
        System.out.println(_p(natural_sort(((String[])(example2)))));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
