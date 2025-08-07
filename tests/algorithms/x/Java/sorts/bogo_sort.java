public class Main {
    static int seed = 0;
    static int[] data;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static int rand_range(int max) {
        return Math.floorMod(rand(), max);
    }

    static int[] shuffle(int[] list_int) {
        int i = list_int.length - 1;
        while (i > 0) {
            int j = rand_range(i + 1);
            int tmp = list_int[i];
list_int[i] = list_int[j];
list_int[j] = tmp;
            i = i - 1;
        }
        return list_int;
    }

    static boolean is_sorted(int[] list_int) {
        int i_1 = 0;
        while (i_1 < list_int.length - 1) {
            if (list_int[i_1] > list_int[i_1 + 1]) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static int[] bogo_sort(int[] list_int) {
        int[] res = ((int[])(list_int));
        while (!(Boolean)is_sorted(((int[])(res)))) {
            res = ((int[])(shuffle(((int[])(res)))));
        }
        return res;
    }
    public static void main(String[] args) {
        seed = 1;
        data = ((int[])(new int[]{3, 2, 1}));
        System.out.println(_p(bogo_sort(((int[])(data)))));
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
