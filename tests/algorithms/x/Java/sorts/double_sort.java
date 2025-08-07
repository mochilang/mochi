public class Main {

    static int[] double_sort(int[] collection) {
        int no_of_elements = collection.length;
        int passes = ((Number)((Math.floorDiv((no_of_elements - 1), 2)))).intValue() + 1;
        int i = 0;
        while (i < passes) {
            int j = 0;
            while (j < no_of_elements - 1) {
                if (collection[j + 1] < collection[j]) {
                    int tmp = collection[j];
collection[j] = collection[j + 1];
collection[j + 1] = tmp;
                }
                if (collection[no_of_elements - 1 - j] < collection[no_of_elements - 2 - j]) {
                    int tmp2 = collection[no_of_elements - 1 - j];
collection[no_of_elements - 1 - j] = collection[no_of_elements - 2 - j];
collection[no_of_elements - 2 - j] = tmp2;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return collection;
    }
    public static void main(String[] args) {
        System.out.println(_p(double_sort(((int[])(new int[]{-1, -2, -3, -4, -5, -6, -7})))));
        System.out.println(_p(double_sort(((int[])(new int[]{})))));
        System.out.println(_p(double_sort(((int[])(new int[]{-1, -2, -3, -4, -5, -6})))));
        System.out.println(_p(double_sort(((int[])(new int[]{-3, 10, 16, -42, 29}))) == new int[]{-42, -3, 10, 16, 29}));
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
