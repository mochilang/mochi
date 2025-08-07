public class Main {
    static int match_in_pattern(String pat, String ch) {
        int i = _runeLen(pat) - 1;
        while (i >= 0) {
            if ((_substr(pat, i, i + 1).equals(ch))) {
                return i;
            }
            i = i - 1;
        }
        return -1;
    }

    static int mismatch_in_text(String text, String pat, int current_pos) {
        int i_1 = _runeLen(pat) - 1;
        while (i_1 >= 0) {
            if (!(_substr(pat, i_1, i_1 + 1).equals(_substr(text, current_pos + i_1, current_pos + i_1 + 1)))) {
                return current_pos + i_1;
            }
            i_1 = i_1 - 1;
        }
        return -1;
    }

    static int[] bad_character_heuristic(String text, String pat) {
        int textLen = _runeLen(text);
        int patLen = _runeLen(pat);
        int[] positions = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 <= textLen - patLen) {
            int mismatch_index = mismatch_in_text(text, pat, i_2);
            if (mismatch_index < 0) {
                positions = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(positions), java.util.stream.IntStream.of(i_2)).toArray()));
                i_2 = i_2 + 1;
            } else {
                String ch = _substr(text, mismatch_index, mismatch_index + 1);
                int match_index = match_in_pattern(pat, ch);
                if (match_index < 0) {
                    i_2 = mismatch_index + 1;
                } else {
                    i_2 = mismatch_index - match_index;
                }
            }
        }
        return positions;
    }
    public static void main(String[] args) {
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
