public class Main {
    static String query;
    static String subject;
    static int[][] score_1;

    static int score_function(String source_char, String target_char, int match_score, int mismatch_score, int gap_score) {
        if ((source_char.equals("-")) || (target_char.equals("-"))) {
            return gap_score;
        }
        if ((source_char.equals(target_char))) {
            return match_score;
        }
        return mismatch_score;
    }

    static int[][] smith_waterman(String query, String subject, int match_score, int mismatch_score, int gap_score) {
        String q = query.toUpperCase();
        String s = subject.toUpperCase();
        int m = _runeLen(q);
        int n = _runeLen(s);
        int[][] score = ((int[][])(new int[][]{}));
        for (int _v = 0; _v < (m + 1); _v++) {
            int[] row = ((int[])(new int[]{}));
            for (int _2 = 0; _2 < (n + 1); _2++) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
            }
            score = ((int[][])(appendObj(score, row)));
        }
        for (int i = 1; i < (m + 1); i++) {
            for (int j = 1; j < (n + 1); j++) {
                String qc = _substr(q, i - 1, i);
                String sc = _substr(s, j - 1, j);
                int diag = score[i - 1][j - 1] + score_function(qc, sc, match_score, mismatch_score, gap_score);
                int delete = score[i - 1][j] + gap_score;
                int insert = score[i][j - 1] + gap_score;
                int max_val = 0;
                if (diag > max_val) {
                    max_val = diag;
                }
                if (delete > max_val) {
                    max_val = delete;
                }
                if (insert > max_val) {
                    max_val = insert;
                }
score[i][j] = max_val;
            }
        }
        return score;
    }

    static String traceback(int[][] score, String query, String subject, int match_score, int mismatch_score, int gap_score) {
        String q_1 = query.toUpperCase();
        String s_1 = subject.toUpperCase();
        int max_value = 0;
        int i_max = 0;
        int j_max = 0;
        for (int i = 0; i < score.length; i++) {
            for (int j = 0; j < score[i].length; j++) {
                if (score[i][j] > max_value) {
                    max_value = score[i][j];
                    i_max = i;
                    j_max = j;
                }
            }
        }
        int i = i_max;
        int j = j_max;
        String align1 = "";
        String align2 = "";
        int gap_penalty = score_function("-", "-", match_score, mismatch_score, gap_score);
        if (i == 0 || j == 0) {
            return "";
        }
        while (i > 0 && j > 0) {
            String qc_1 = _substr(q_1, i - 1, i);
            String sc_1 = _substr(s_1, j - 1, j);
            if (score[i][j] == score[i - 1][j - 1] + score_function(qc_1, sc_1, match_score, mismatch_score, gap_score)) {
                align1 = qc_1 + align1;
                align2 = sc_1 + align2;
                i = i - 1;
                j = j - 1;
            } else             if (score[i][j] == score[i - 1][j] + gap_penalty) {
                align1 = qc_1 + align1;
                align2 = "-" + align2;
                i = i - 1;
            } else {
                align1 = "-" + align1;
                align2 = sc_1 + align2;
                j = j - 1;
            }
        }
        return align1 + "\n" + align2;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            query = "HEAGAWGHEE";
            subject = "PAWHEAE";
            score_1 = ((int[][])(smith_waterman(query, subject, 1, -1, -2)));
            System.out.println(traceback(((int[][])(score_1)), query, subject, 1, -1, -2));
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
}
