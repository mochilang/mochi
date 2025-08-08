public class Main {

    static java.util.Map<String,Integer> word_occurrence(String sentence) {
        java.util.Map<String,Integer> occurrence = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        String word = "";
        int i = 0;
        while (i < _runeLen(sentence)) {
            String ch = _substr(sentence, i, i + 1);
            if ((ch.equals(" "))) {
                if (!(word.equals(""))) {
                    if (((Boolean)(occurrence.containsKey(word)))) {
occurrence.put(word, (int)(((int)(occurrence).getOrDefault(word, 0))) + 1);
                    } else {
occurrence.put(word, 1);
                    }
                    word = "";
                }
            } else {
                word = word + ch;
            }
            i = i + 1;
        }
        if (!(word.equals(""))) {
            if (((Boolean)(occurrence.containsKey(word)))) {
occurrence.put(word, (int)(((int)(occurrence).getOrDefault(word, 0))) + 1);
            } else {
occurrence.put(word, 1);
            }
        }
        return occurrence;
    }

    static void main() {
        java.util.Map<String,Integer> result = word_occurrence("INPUT STRING");
        for (String w : result.keySet()) {
            System.out.println(w + ": " + _p(((int)(result).getOrDefault(w, 0))));
        }
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
