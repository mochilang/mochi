public class Main {

    static int[] insertion_sort(int[] xs) {
        int i = 1;
        while (i < xs.length) {
            int value = xs[i];
            int j = i - 1;
            while (j >= 0 && xs[j] > value) {
xs[j + 1] = xs[j];
                j = j - 1;
            }
xs[j + 1] = value;
            i = i + 1;
        }
        return xs;
    }
    public static void main(String[] args) {
        System.out.println(_p(insertion_sort(((int[])(new int[]{0, 5, 3, 2, 2})))));
        System.out.println(_p(insertion_sort(((int[])(new int[]{})))));
        System.out.println(_p(insertion_sort(((int[])(new int[]{-2, -5, -45})))));
        System.out.println(_p(insertion_sort(((int[])(new int[]{3})))));
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
