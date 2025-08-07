public class Main {

    static int[] bead_sort(int[] sequence) {
        int n = sequence.length;
        int i = 0;
        while (i < n) {
            if (sequence[i] < 0) {
                throw new RuntimeException(String.valueOf("Sequence must be list of non-negative integers"));
            }
            i = i + 1;
        }
        int pass = 0;
        while (pass < n) {
            int j = 0;
            while (j < n - 1) {
                int upper = sequence[j];
                int lower = sequence[j + 1];
                if (upper > lower) {
                    int diff = upper - lower;
sequence[j] = upper - diff;
sequence[j + 1] = lower + diff;
                }
                j = j + 1;
            }
            pass = pass + 1;
        }
        return sequence;
    }
    public static void main(String[] args) {
        System.out.println(_p(bead_sort(((int[])(new int[]{6, 11, 12, 4, 1, 5})))));
        System.out.println(_p(bead_sort(((int[])(new int[]{9, 8, 7, 6, 5, 4, 3, 2, 1})))));
        System.out.println(_p(bead_sort(((int[])(new int[]{5, 0, 4, 3})))));
        System.out.println(_p(bead_sort(((int[])(new int[]{8, 2, 1})))));
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
