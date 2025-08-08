public class Main {

    static int pivot(int[] lst) {
        return lst[0];
    }

    static int kth_number(int[] lst, int k) {
        int p = pivot(((int[])(lst)));
        int[] small = ((int[])(new int[]{}));
        int[] big = ((int[])(new int[]{}));
        int i = 0;
        while (i < lst.length) {
            int e = lst[i];
            if (e < p) {
                small = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(small), java.util.stream.IntStream.of(e)).toArray()));
            } else             if (e > p) {
                big = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(big), java.util.stream.IntStream.of(e)).toArray()));
            }
            i = i + 1;
        }
        if (small.length == k - 1) {
            return p;
        } else         if (small.length < k - 1) {
            return kth_number(((int[])(big)), k - small.length - 1);
        } else {
            return kth_number(((int[])(small)), k);
        }
    }
    public static void main(String[] args) {
        System.out.println(_p(kth_number(((int[])(new int[]{2, 1, 3, 4, 5})), 3)));
        System.out.println(_p(kth_number(((int[])(new int[]{2, 1, 3, 4, 5})), 1)));
        System.out.println(_p(kth_number(((int[])(new int[]{2, 1, 3, 4, 5})), 5)));
        System.out.println(_p(kth_number(((int[])(new int[]{3, 2, 5, 6, 7, 8})), 2)));
        System.out.println(_p(kth_number(((int[])(new int[]{25, 21, 98, 100, 76, 22, 43, 60, 89, 87})), 4)));
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
