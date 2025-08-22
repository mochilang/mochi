public class Main {

    static long[] triangular_numbers(long limit) {
        long[] res = ((long[])(new long[]{}));
        long n_1 = 1L;
        while ((long)(n_1) <= (long)(limit)) {
            res = ((long[])(appendLong(res, Math.floorDiv(((long)(n_1) * (long)(((long)(n_1) + 1L))), 2))));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return res;
    }

    static String[] parse_words(String text) {
        String[] words = ((String[])(new String[]{}));
        String current_1 = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(_runeLen(text))) {
            String c_1 = _substr(text, (int)((long)(i_1)), (int)((long)((long)(i_1) + 1L)));
            if ((c_1.equals(","))) {
                words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else             if ((c_1.equals("\""))) {
            } else             if ((c_1.equals("\r")) || (c_1.equals("\n"))) {
            } else {
                current_1 = current_1 + c_1;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        if ((long)(_runeLen(current_1)) > 0L) {
            words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        }
        return words;
    }

    static long word_value(String word) {
        long total = 0L;
        long i_3 = 0L;
        while ((long)(i_3) < (long)(_runeLen(word))) {
            total = (long)((long)((long)(total) + (long)(_ord(_substr(word, (int)((long)(i_3)), (int)((long)((long)(i_3) + 1L)))))) - 64L);
            i_3 = (long)((long)(i_3) + 1L);
        }
        return total;
    }

    static boolean contains(long[] xs, long target) {
        for (long x : xs) {
            if ((long)(x) == (long)(target)) {
                return true;
            }
        }
        return false;
    }

    static long solution() {
        String text = String.valueOf(read_file("words.txt"));
        String[] words_2 = ((String[])(parse_words(text)));
        long[] tri_1 = ((long[])(triangular_numbers(100L)));
        long count_1 = 0L;
        for (String w : words_2) {
            long v_1 = (long)(word_value(w));
            if (contains(((long[])(tri_1)), (long)(v_1))) {
                count_1 = (long)((long)(count_1) + 1L);
            }
        }
        return count_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution()));
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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

    static final String _dataDir = "project_euler/problem_042";
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

    static int _ord(String s) {
        return s.codePointAt(0);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
