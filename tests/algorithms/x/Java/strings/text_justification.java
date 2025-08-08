public class Main {

    static String repeat_str(String s, int count) {
        String res = "";
        int i = 0;
        while (i < count) {
            res = res + s;
            i = i + 1;
        }
        return res;
    }

    static String[] split_words(String s) {
        String[] res_1 = ((String[])(new String[]{}));
        String current = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = s.substring(i_1, i_1 + 1);
            if ((ch.equals(" "))) {
                if (!(current.equals(""))) {
                    res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                    current = "";
                }
            } else {
                current = current + ch;
            }
            i_1 = i_1 + 1;
        }
        if (!(current.equals(""))) {
            res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        }
        return res_1;
    }

    static String justify_line(String[] line, int width, int max_width) {
        int overall_spaces_count = max_width - width;
        int words_count = line.length;
        if (words_count == 1) {
            return line[0] + String.valueOf(repeat_str(" ", overall_spaces_count));
        }
        int spaces_to_insert_between_words = words_count - 1;
        int[] num_spaces_between_words_list = ((int[])(new int[]{}));
        int base = Math.floorDiv(overall_spaces_count, spaces_to_insert_between_words);
        int extra = Math.floorMod(overall_spaces_count, spaces_to_insert_between_words);
        int i_2 = 0;
        while (i_2 < spaces_to_insert_between_words) {
            int spaces = base;
            if (i_2 < extra) {
                spaces = spaces + 1;
            }
            num_spaces_between_words_list = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(num_spaces_between_words_list), java.util.stream.IntStream.of(spaces)).toArray()));
            i_2 = i_2 + 1;
        }
        String aligned = "";
        i_2 = 0;
        while (i_2 < spaces_to_insert_between_words) {
            aligned = aligned + line[i_2] + String.valueOf(repeat_str(" ", num_spaces_between_words_list[i_2]));
            i_2 = i_2 + 1;
        }
        aligned = aligned + line[spaces_to_insert_between_words];
        return aligned;
    }

    static String[] text_justification(String word, int max_width) {
        String[] words = ((String[])(split_words(word)));
        String[] answer = ((String[])(new String[]{}));
        String[] line = ((String[])(new String[]{}));
        int width = 0;
        int idx = 0;
        while (idx < words.length) {
            String w = words[idx];
            if (width + _runeLen(w) + line.length <= max_width) {
                line = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(line), java.util.stream.Stream.of(w)).toArray(String[]::new)));
                width = width + _runeLen(w);
            } else {
                answer = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(answer), java.util.stream.Stream.of(justify_line(((String[])(line)), width, max_width))).toArray(String[]::new)));
                line = ((String[])(new String[]{w}));
                width = _runeLen(w);
            }
            idx = idx + 1;
        }
        int remaining_spaces = max_width - width - line.length;
        String last_line = "";
        int j = 0;
        while (j < line.length) {
            if (j > 0) {
                last_line = last_line + " ";
            }
            last_line = last_line + line[j];
            j = j + 1;
        }
        last_line = last_line + String.valueOf(repeat_str(" ", remaining_spaces + 1));
        answer = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(answer), java.util.stream.Stream.of(last_line)).toArray(String[]::new)));
        return answer;
    }
    public static void main(String[] args) {
        System.out.println(_p(text_justification("This is an example of text justification.", 16)));
        System.out.println(_p(text_justification("Two roads diverged in a yellow wood", 16)));
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
