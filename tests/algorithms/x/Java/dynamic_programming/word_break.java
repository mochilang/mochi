public class Main {

    static java.util.Map<String,Boolean> build_set(String[] words) {
        java.util.Map<String,Boolean> m = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        for (String w : words) {
m.put(w, true);
        }
        return m;
    }

    static boolean word_break(String s, String[] words) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(_runeLen(s)));
        java.util.Map<String,Boolean> dict_1 = build_set(((String[])(words)));
        boolean[] dp_1 = ((boolean[])(new boolean[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) <= 0) {
            dp_1 = ((boolean[])(appendBool(dp_1, false)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
dp_1[(int)(0L)] = true;
        i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(n) <= 0) {
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(i_1) < 0) {
                if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(j_1)).longValue())]) {
                    String sub_1 = _substr(s, (int)(((java.math.BigInteger)(j_1)).longValue()), (int)(((java.math.BigInteger)(i_1)).longValue()));
                    if (dict_1.containsKey(sub_1)) {
dp_1[(int)(((java.math.BigInteger)(i_1)).longValue())] = true;
                        j_1 = new java.math.BigInteger(String.valueOf(i_1));
                    }
                }
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return dp_1[_idx((dp_1).length, ((java.math.BigInteger)(n)).longValue())];
    }

    static void print_bool(boolean b) {
        if (b) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        print_bool(word_break("applepenapple", ((String[])(new String[]{"apple", "pen"}))));
        print_bool(word_break("catsandog", ((String[])(new String[]{"cats", "dog", "sand", "and", "cat"}))));
        print_bool(word_break("cars", ((String[])(new String[]{"car", "ca", "rs"}))));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
