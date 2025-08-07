public class Main {

    static int[][] partition(int[] data, int pivot) {
        int[] less = ((int[])(new int[]{}));
        int[] equal = ((int[])(new int[]{}));
        int[] greater = ((int[])(new int[]{}));
        for (int i = 0; i < data.length; i++) {
            int v = data[i];
            if (v < pivot) {
                less = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(less), java.util.stream.IntStream.of(v)).toArray()));
            } else             if (v > pivot) {
                greater = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(greater), java.util.stream.IntStream.of(v)).toArray()));
            } else {
                equal = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(equal), java.util.stream.IntStream.of(v)).toArray()));
            }
        }
        return new int[][]{less, equal, greater};
    }

    static int quick_select(int[] items, int index) {
        if (index < 0 || index >= items.length) {
            return -1;
        }
        int pivot = items[Math.floorDiv(items.length, 2)];
        int[][] parts = ((int[][])(partition(((int[])(items)), pivot)));
        int[] smaller = ((int[])(parts[0]));
        int[] equal_1 = ((int[])(parts[1]));
        int[] larger = ((int[])(parts[2]));
        int count = equal_1.length;
        int m = smaller.length;
        if (m <= index && index < m + count) {
            return pivot;
        } else         if (index < m) {
            return quick_select(((int[])(smaller)), index);
        } else {
            return quick_select(((int[])(larger)), index - (m + count));
        }
    }

    static double median(int[] items) {
        int n = items.length;
        int mid = Math.floorDiv(n, 2);
        if (Math.floorMod(n, 2) != 0) {
            return 1.0 * quick_select(((int[])(items)), mid);
        } else {
            int low = quick_select(((int[])(items)), mid - 1);
            int high = quick_select(((int[])(items)), mid);
            return (1.0 * (low + high)) / 2.0;
        }
    }
    public static void main(String[] args) {
        System.out.println(_p(quick_select(((int[])(new int[]{2, 4, 5, 7, 899, 54, 32})), 5)));
        System.out.println(_p(quick_select(((int[])(new int[]{2, 4, 5, 7, 899, 54, 32})), 1)));
        System.out.println(_p(quick_select(((int[])(new int[]{5, 4, 3, 2})), 2)));
        System.out.println(_p(quick_select(((int[])(new int[]{3, 5, 7, 10, 2, 12})), 3)));
        System.out.println(_p(median(((int[])(new int[]{3, 2, 2, 9, 9})))));
        System.out.println(_p(median(((int[])(new int[]{2, 2, 9, 9, 9, 3})))));
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
