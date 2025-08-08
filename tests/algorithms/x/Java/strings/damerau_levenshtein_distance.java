public class Main {

    static int damerau_levenshtein_distance(String first_string, String second_string) {
        int len1 = _runeLen(first_string);
        int len2 = _runeLen(second_string);
        int[][] dp_matrix = ((int[][])(new int[][]{}));
        for (int _v = 0; _v < (len1 + 1); _v++) {
            int[] row = ((int[])(new int[]{}));
            for (int _2 = 0; _2 < (len2 + 1); _2++) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
            }
            dp_matrix = ((int[][])(appendObj(dp_matrix, row)));
        }
        for (int i = 0; i < (len1 + 1); i++) {
            int[] row_1 = ((int[])(dp_matrix[i]));
row_1[0] = i;
dp_matrix[i] = ((int[])(row_1));
        }
        int[] first_row = ((int[])(dp_matrix[0]));
        for (int j = 0; j < (len2 + 1); j++) {
first_row[j] = j;
        }
dp_matrix[0] = ((int[])(first_row));
        for (int i = 1; i < (len1 + 1); i++) {
            int[] row_2 = ((int[])(dp_matrix[i]));
            String first_char = _substr(first_string, i - 1, i);
            for (int j = 1; j < (len2 + 1); j++) {
                String second_char = _substr(second_string, j - 1, j);
                int cost = (first_char.equals(second_char)) ? 0 : 1;
                int value = dp_matrix[i - 1][j] + 1;
                int insertion = row_2[j - 1] + 1;
                if (insertion < value) {
                    value = insertion;
                }
                int substitution = dp_matrix[i - 1][j - 1] + cost;
                if (substitution < value) {
                    value = substitution;
                }
row_2[j] = value;
                if (i > 1 && j > 1 && (_substr(first_string, i - 1, i).equals(_substr(second_string, j - 2, j - 1))) && (_substr(first_string, i - 2, i - 1).equals(_substr(second_string, j - 1, j)))) {
                    int transposition = dp_matrix[i - 2][j - 2] + cost;
                    if (transposition < row_2[j]) {
row_2[j] = transposition;
                    }
                }
            }
dp_matrix[i] = ((int[])(row_2));
        }
        return dp_matrix[len1][len2];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(damerau_levenshtein_distance("cat", "cut")));
            System.out.println(_p(damerau_levenshtein_distance("kitten", "sitting")));
            System.out.println(_p(damerau_levenshtein_distance("hello", "world")));
            System.out.println(_p(damerau_levenshtein_distance("book", "back")));
            System.out.println(_p(damerau_levenshtein_distance("container", "containment")));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
