public class Main {
    static int[] seq1;
    static int[] seq2 = new int[0];
    static int[] seq3;
    static int[] seq4;
    static int[] seq5;
    static int[] seq6;
    static int[] seq7;
    static int[] seq8;

    static void swap(int[] seq, int i, int j) {
        int temp = seq[i];
seq[i] = seq[j];
seq[j] = temp;
    }

    static void slowsort_recursive(int[] seq, int start, int end_index) {
        if (start >= end_index) {
            return;
        }
        int mid = Math.floorDiv((start + end_index), 2);
        slowsort_recursive(((int[])(seq)), start, mid);
        slowsort_recursive(((int[])(seq)), mid + 1, end_index);
        if (seq[end_index] < seq[mid]) {
            swap(((int[])(seq)), end_index, mid);
        }
        slowsort_recursive(((int[])(seq)), start, end_index - 1);
    }

    static int[] slow_sort(int[] seq) {
        if (seq.length > 0) {
            slowsort_recursive(((int[])(seq)), 0, seq.length - 1);
        }
        return seq;
    }
    public static void main(String[] args) {
        seq1 = ((int[])(new int[]{1, 6, 2, 5, 3, 4, 4, 5}));
        System.out.println(_p(slow_sort(((int[])(seq1)))));
        seq2 = ((int[])(new int[]{}));
        System.out.println(_p(slow_sort(((int[])(seq2)))));
        seq3 = ((int[])(new int[]{2}));
        System.out.println(_p(slow_sort(((int[])(seq3)))));
        seq4 = ((int[])(new int[]{1, 2, 3, 4}));
        System.out.println(_p(slow_sort(((int[])(seq4)))));
        seq5 = ((int[])(new int[]{4, 3, 2, 1}));
        System.out.println(_p(slow_sort(((int[])(seq5)))));
        seq6 = ((int[])(new int[]{9, 8, 7, 6, 5, 4, 3, 2, 1, 0}));
        slowsort_recursive(((int[])(seq6)), 2, 7);
        System.out.println(_p(seq6));
        seq7 = ((int[])(new int[]{9, 8, 7, 6, 5, 4, 3, 2, 1, 0}));
        slowsort_recursive(((int[])(seq7)), 0, 4);
        System.out.println(_p(seq7));
        seq8 = ((int[])(new int[]{9, 8, 7, 6, 5, 4, 3, 2, 1, 0}));
        slowsort_recursive(((int[])(seq8)), 5, seq8.length - 1);
        System.out.println(_p(seq8));
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
