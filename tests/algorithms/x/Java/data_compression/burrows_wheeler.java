public class Main {
    static class BWTResult {
        String bwt_string;
        int idx_original_string;
        BWTResult(String bwt_string, int idx_original_string) {
            this.bwt_string = bwt_string;
            this.idx_original_string = idx_original_string;
        }
        BWTResult() {}
        @Override public String toString() {
            return String.format("{'bwt_string': '%s', 'idx_original_string': %s}", String.valueOf(bwt_string), String.valueOf(idx_original_string));
        }
    }

    static String s;
    static BWTResult result;

    static String[] all_rotations(String s) {
        int n = _runeLen(s);
        String[] rotations = ((String[])(new String[]{}));
        int i = 0;
        while (i < n) {
            String rotation = _substr(s, i, n) + _substr(s, 0, i);
            rotations = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rotations), java.util.stream.Stream.of(rotation)).toArray(String[]::new)));
            i = i + 1;
        }
        return rotations;
    }

    static String[] sort_strings(String[] arr) {
        int n_1 = arr.length;
        int i_1 = 1;
        while (i_1 < n_1) {
            String key = arr[i_1];
            int j = i_1 - 1;
            while (j >= 0 && (arr[j].compareTo(key) > 0)) {
arr[j + 1] = arr[j];
                j = j - 1;
            }
arr[j + 1] = key;
            i_1 = i_1 + 1;
        }
        return arr;
    }

    static String join_strings(String[] arr) {
        String res = "";
        int i_2 = 0;
        while (i_2 < arr.length) {
            res = res + arr[i_2];
            i_2 = i_2 + 1;
        }
        return res;
    }

    static BWTResult bwt_transform(String s) {
        if ((s.equals(""))) {
            throw new RuntimeException(String.valueOf("input string must not be empty"));
        }
        String[] rotations_1 = ((String[])(all_rotations(s)));
        rotations_1 = ((String[])(sort_strings(((String[])(rotations_1)))));
        String[] last_col = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < rotations_1.length) {
            String word = rotations_1[i_3];
            last_col = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(last_col), java.util.stream.Stream.of(_substr(word, _runeLen(word) - 1, _runeLen(word)))).toArray(String[]::new)));
            i_3 = i_3 + 1;
        }
        String bwt_string = String.valueOf(join_strings(((String[])(last_col))));
        int idx = index_of(((String[])(rotations_1)), s);
        return new BWTResult(bwt_string, idx);
    }

    static int index_of(String[] arr, String target) {
        int i_4 = 0;
        while (i_4 < arr.length) {
            if ((arr[i_4].equals(target))) {
                return i_4;
            }
            i_4 = i_4 + 1;
        }
        return -1;
    }

    static String reverse_bwt(String bwt_string, int idx_original_string) {
        if ((bwt_string.equals(""))) {
            throw new RuntimeException(String.valueOf("bwt string must not be empty"));
        }
        int n_2 = _runeLen(bwt_string);
        if (idx_original_string < 0 || idx_original_string >= n_2) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        String[] ordered_rotations = ((String[])(new String[]{}));
        int i_5 = 0;
        while (i_5 < n_2) {
            ordered_rotations = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ordered_rotations), java.util.stream.Stream.of("")).toArray(String[]::new)));
            i_5 = i_5 + 1;
        }
        int iter = 0;
        while (iter < n_2) {
            int j_1 = 0;
            while (j_1 < n_2) {
                String ch = _substr(bwt_string, j_1, j_1 + 1);
ordered_rotations[j_1] = ch + ordered_rotations[j_1];
                j_1 = j_1 + 1;
            }
            ordered_rotations = ((String[])(sort_strings(((String[])(ordered_rotations)))));
            iter = iter + 1;
        }
        return ordered_rotations[idx_original_string];
    }
    public static void main(String[] args) {
        s = "^BANANA";
        result = bwt_transform(s);
        System.out.println(result.bwt_string);
        System.out.println(result.idx_original_string);
        System.out.println(reverse_bwt(result.bwt_string, result.idx_original_string));
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
