public class Main {
    static int[] longest_subsequence(int[] xs) {
        int n = xs.length;
        if (n <= 1) {
            return xs;
        }
        int pivot = xs[0];
        boolean is_found = false;
        int i = 1;
        int[] longest_subseq = ((int[])(new int[]{}));
        while (!is_found && i < n) {
            if (xs[i] < pivot) {
                is_found = true;
                int[] temp_array = ((int[])(java.util.Arrays.copyOfRange(xs, i, n)));
                temp_array = ((int[])(longest_subsequence(((int[])(temp_array)))));
                if (temp_array.length > longest_subseq.length) {
                    longest_subseq = ((int[])(temp_array));
                }
            } else {
                i = i + 1;
            }
        }
        int[] filtered = ((int[])(new int[]{}));
        int j = 1;
        while (j < n) {
            if (xs[j] >= pivot) {
                filtered = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(filtered), java.util.stream.IntStream.of(xs[j])).toArray()));
            }
            j = j + 1;
        }
        int[] candidate = ((int[])(new int[]{}));
        candidate = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(candidate), java.util.stream.IntStream.of(pivot)).toArray()));
        candidate = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(candidate), java.util.Arrays.stream(longest_subsequence(((int[])(filtered))))).toArray()));
        if (candidate.length > longest_subseq.length) {
            return candidate;
        } else {
            return longest_subseq;
        }
    }
    public static void main(String[] args) {
    }
}
