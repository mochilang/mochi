public class Main {

    static int[] range_list(int n) {
        int[] lst = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            lst = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lst), java.util.stream.IntStream.of(i)).toArray()));
            i = i + 1;
        }
        return lst;
    }

    static int min3(int a, int b, int c) {
        int m = a;
        if (b < m) {
            m = b;
        }
        if (c < m) {
            m = c;
        }
        return m;
    }

    static int levenshtein_distance(String first_word, String second_word) {
        if (_runeLen(first_word) < _runeLen(second_word)) {
            return levenshtein_distance(second_word, first_word);
        }
        if (_runeLen(second_word) == 0) {
            return _runeLen(first_word);
        }
        int[] previous_row = ((int[])(range_list(_runeLen(second_word) + 1)));
        int i_1 = 0;
        while (i_1 < _runeLen(first_word)) {
            String c1 = first_word.substring(i_1, i_1+1);
            int[] current_row = ((int[])(new int[]{}));
            current_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current_row), java.util.stream.IntStream.of(i_1 + 1)).toArray()));
            int j = 0;
            while (j < _runeLen(second_word)) {
                String c2 = second_word.substring(j, j+1);
                int insertions = previous_row[j + 1] + 1;
                int deletions = current_row[j] + 1;
                int substitutions = previous_row[j] + ((c1.equals(c2)) ? 0 : 1);
                int min_val = min3(insertions, deletions, substitutions);
                current_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current_row), java.util.stream.IntStream.of(min_val)).toArray()));
                j = j + 1;
            }
            previous_row = ((int[])(current_row));
            i_1 = i_1 + 1;
        }
        return previous_row[previous_row.length - 1];
    }

    static int levenshtein_distance_optimized(String first_word, String second_word) {
        if (_runeLen(first_word) < _runeLen(second_word)) {
            return levenshtein_distance_optimized(second_word, first_word);
        }
        if (_runeLen(second_word) == 0) {
            return _runeLen(first_word);
        }
        int[] previous_row_1 = ((int[])(range_list(_runeLen(second_word) + 1)));
        int i_2 = 0;
        while (i_2 < _runeLen(first_word)) {
            String c1_1 = first_word.substring(i_2, i_2+1);
            int[] current_row_1 = ((int[])(new int[]{}));
            current_row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current_row_1), java.util.stream.IntStream.of(i_2 + 1)).toArray()));
            int k = 0;
            while (k < _runeLen(second_word)) {
                current_row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current_row_1), java.util.stream.IntStream.of(0)).toArray()));
                k = k + 1;
            }
            int j_1 = 0;
            while (j_1 < _runeLen(second_word)) {
                String c2_1 = second_word.substring(j_1, j_1+1);
                int insertions_1 = previous_row_1[j_1 + 1] + 1;
                int deletions_1 = current_row_1[j_1] + 1;
                int substitutions_1 = previous_row_1[j_1] + ((c1_1.equals(c2_1)) ? 0 : 1);
                int min_val_1 = min3(insertions_1, deletions_1, substitutions_1);
current_row_1[j_1 + 1] = min_val_1;
                j_1 = j_1 + 1;
            }
            previous_row_1 = ((int[])(current_row_1));
            i_2 = i_2 + 1;
        }
        return previous_row_1[previous_row_1.length - 1];
    }

    static void main() {
        String a = "kitten";
        String b = "sitting";
        System.out.println(_p(levenshtein_distance(a, b)));
        System.out.println(_p(levenshtein_distance_optimized(a, b)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
