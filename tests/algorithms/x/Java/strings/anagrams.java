public class Main {
    static java.util.Map<String,String[]> word_by_signature = null;

    static String[] split(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        String current_1 = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(_runeLen(s))) {
            String ch_1 = _substr(s, (int)((long)(i_1)), (int)((long)((long)(i_1) + 1L)));
            if ((ch_1.equals(sep))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else {
                current_1 = current_1 + ch_1;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        return res;
    }

    static String[] insertion_sort(String[] arr) {
        String[] a = ((String[])(arr));
        long i_3 = 1L;
        while ((long)(i_3) < (long)(a.length)) {
            String key_1 = a[(int)((long)(i_3))];
            long j_1 = (long)((long)(i_3) - 1L);
            while ((long)(j_1) >= 0L && (a[(int)((long)(j_1))].compareTo(key_1) > 0)) {
a[(int)((long)((long)(j_1) + 1L))] = a[(int)((long)(j_1))];
                j_1 = (long)((long)(j_1) - 1L);
            }
a[(int)((long)((long)(j_1) + 1L))] = key_1;
            i_3 = (long)((long)(i_3) + 1L);
        }
        return a;
    }

    static String sort_chars(String word) {
        String[] chars = ((String[])(new String[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(_runeLen(word))) {
            chars = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(_substr(word, (int)((long)(i_5)), (int)((long)((long)(i_5) + 1L))))).toArray(String[]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        chars = ((String[])(insertion_sort(((String[])(chars)))));
        String res_2 = "";
        i_5 = 0L;
        while ((long)(i_5) < (long)(chars.length)) {
            res_2 = res_2 + chars[(int)((long)(i_5))];
            i_5 = (long)((long)(i_5) + 1L);
        }
        return res_2;
    }

    static String[] unique_sorted(String[] words) {
        java.util.Map<String,Boolean> seen = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        String[] res_4 = ((String[])(new String[]{}));
        for (String w : words) {
            if (!(w.equals("")) && !seen.containsKey(w)) {
                res_4 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_4), java.util.stream.Stream.of(w)).toArray(String[]::new)));
seen.put(w, true);
            }
        }
        res_4 = ((String[])(insertion_sort(((String[])(res_4)))));
        return res_4;
    }

    static void build_map(String[] words) {
        for (String w : words) {
            String sig = String.valueOf(sort_chars(w));
            String[] arr = ((String[])(new String[]{}));
            if (word_by_signature.containsKey(sig)) {
                arr = (String[])(((String[])(word_by_signature).get(sig)));
            }
            arr = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(w)).toArray(String[]::new)));
word_by_signature.put(sig, ((String[])(arr)));
        }
    }

    static String[] anagram(String my_word) {
        String sig_1 = String.valueOf(sort_chars(my_word));
        if (word_by_signature.containsKey(sig_1)) {
            return ((String[])(word_by_signature).get(sig_1));
        }
        return new String[]{};
    }

    static void main() {
        String text = String.valueOf(read_file("words.txt"));
        String[] lines_1 = ((String[])(text.split(java.util.regex.Pattern.quote("\n"))));
        String[] words_1 = ((String[])(unique_sorted(((String[])(lines_1)))));
        build_map(((String[])(words_1)));
        for (String w : words_1) {
            String[] anas_1 = ((String[])(anagram(w)));
            if ((long)(anas_1.length) > 1L) {
                String line_1 = w + ":";
                long i_7 = 0L;
                while ((long)(i_7) < (long)(anas_1.length)) {
                    if ((long)(i_7) > 0L) {
                        line_1 = line_1 + ",";
                    }
                    line_1 = line_1 + anas_1[(int)((long)(i_7))];
                    i_7 = (long)((long)(i_7) + 1L);
                }
                System.out.println(line_1);
            }
        }
    }
    public static void main(String[] args) {
        word_by_signature = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        main();
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

    static final String _dataDir = "strings";
    static String read_file(String path) {
        java.io.File f = new java.io.File(path);
        if (!f.isAbsolute()) {
            String root = System.getenv("MOCHI_ROOT");
            if (root != null && !root.isEmpty()) {
                java.io.File alt = new java.io.File(root + java.io.File.separator + "tests" + java.io.File.separator + "github" + java.io.File.separator + "TheAlgorithms" + java.io.File.separator + "Mochi" + java.io.File.separator + _dataDir, path);
                if (alt.exists()) f = alt;
            }
        }
        try {
            return new String(java.nio.file.Files.readAllBytes(f.toPath()));
        } catch (Exception e) {
            return "";
        }
    }
}
